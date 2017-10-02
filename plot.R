library(tidyverse)
library(gganimate)
library(ggforce)
library(scales)
animation::ani.options(loop = 0)
theme_set(theme_minimal())

parties2017 <- c("Linke", "Grüne", "SPD", "FDP", "Union", "AfD")
party_labels <- paste0(parties2017, "    ")
n_parties <- length(parties2017)

seats2017 <- c(69, 67, 153, 80, 246, 94)
names(seats2017) <- parties2017

party_colors <- c("#BE3075", "#78BC1B", "#D71F1D", "#FFCC00", "#121212", "#4076C2")
names(party_colors) <- parties2017

n_seats <- sum(seats2017)
max_frame <- 94 / 2
n_frames <- max_frame + 1

n_parts <- round(n_frames)
extended_parties <- c("Linke", "Grüne", "SPD", "FDP", "Union", "AfD", as.character(1:n_parts))

seat_shift <- function(frame, by = 1, seats = seats2017) {
    seats["Union"] <- seats["Union"] + by*frame
    seats["AfD"] <- seats["AfD"] - by*frame
    seats
}

all_seats <- rep(seats2017, times = n_frames)
for (i in 1:max_frame) {
    all_seats[(i*n_parties + 1):((i+1)*n_parties)] <- seat_shift(i, by = 2)
}

df1 <- tibble(
    party = rep(parties2017, n_frames),
    seats = all_seats,
    color = rep(party_colors, n_frames),
    frame = rep(1:n_frames, each = n_parties),
    subplot = "Was die Union zu tun glaubt"
) %>% mutate(
    party = factor(party, levels = extended_parties,
                   labels = paste0(extended_parties, "    ")),
    share = seats / n_seats
) %>% 
    group_by(frame) %>% 
    mutate(
        ymax = cumsum(share),
        ymin = c(0, head(ymax, n = -1))
    ) %>% 
    mutate_at(vars(starts_with("y")), rescale, to = pi*c(-.5, .5), from = 0:1)


(p1 <- ggplot(df1, aes(x0 = 0, y0 = 0, r0 = .5, r = 1, start = ymin, end = ymax, 
                       fill = color, frame = frame)) + 
        geom_arc_bar(col = "white", size = 2) + 
        scale_fill_identity("", labels = party_labels, breaks = party_colors,
                            guide = "legend") +
        ggtitle("Was die Union zu tun glaubt") +
        coord_fixed() +
        guides(fill = guide_legend(nrow = 1)) +
        theme(axis.title = element_blank(),
              axis.text = element_blank(),
              panel.grid = element_blank(),
              plot.title = element_text(size = 20, hjust = .5, face = "bold"),
              legend.title = element_blank(),
              legend.text = element_text(size = 14),
              legend.position = "bottom",
              legend.direction = "horizontal"))

# gganimate(p1, interval = .2, title_frame = FALSE, filename = "was-die-union-zu-tun-glaubt.gif")


df2 <- tibble(
    party = rep(parties2017, n_frames),
    seats = rep(seats2017, n_frames),
    color = rep(party_colors, n_frames),
    frame = rep(1:n_frames, each = n_parties),
    subplot  = "Was wirklich passiert"
) %>% mutate(
    party = factor(party, levels = extended_parties,
                   labels = paste0(extended_parties, "    ")),
    share = seats / n_seats
) %>% 
    group_by(frame) %>% 
    mutate(
        ymax = cumsum(share),
        ymin = c(0, head(ymax, n = -1))
    ) %>% 
    mutate_at(vars(starts_with("y")), rescale, to = pi*c(-.5, .5), from = 0:1)


colfunc <- colorRampPalette(c(party_colors["Union"], party_colors["AfD"]))
parts_colors <- c(rep(party_colors["Union"], n_parts - 8), 
                  colfunc(n_parts), 
                  rep(party_colors["AfD"], n_parts + 8))

all_parts_colors <- rep(party_colors["Union"], times = n_parts * n_frames)
for (i in 1:max_frame) {
    all_parts_colors[(i*n_parts + 1):((i+1)*n_parts)] <- parts_colors[(i+2):(i+n_parts+1)]
}

df3 <- tibble(
    party = as.character(rep(1:n_parts, n_frames)),
    color = all_parts_colors,
    seats = rep(246/n_parts, n_parts * n_frames),
    frame = rep(1:n_frames, each = n_parts),
    subplot = "Was wirklich passiert"
) %>% 
    mutate(
        party = factor(party, levels = extended_parties),
        share = seats / n_seats
    ) %>% 
    group_by(frame) %>% 
    mutate(
        ymax = cumsum(share),
        ymin = c(0, head(ymax, n = -1))
    ) %>% 
    mutate_at(vars(starts_with("y")), rescale, to = c(0.1, 1.145), from = c(0, 0.3469676)
    )

(p2 <- ggplot(df2, aes(x0 = 0, y0 = 0, start = ymin, end = ymax, 
                       r0 = .5, r = 1, fill = color, frame = frame)) + 
        geom_arc_bar(col = "white", size = 2) + 
        geom_arc_bar(data = df3, aes(r0 = 0.5075, r = 0.9925),
                     col = NA, show.legend = FALSE) +
        scale_fill_identity("", labels = party_labels, breaks = party_colors,
                            guide = "legend") +
        ggtitle("Was wirklich passiert") +
        coord_fixed() +
        guides(fill = guide_legend(nrow = 1)) +
        theme(axis.title = element_blank(),
              axis.text = element_blank(),
              panel.grid = element_blank(),
              plot.title = element_text(size = 20, hjust = .5, face = "bold"),
              legend.title = element_blank(),
              legend.text = element_text(size = 14),
              legend.position = "bottom",
              legend.direction = "horizontal"))

# gganimate(p2, interval = .1, title_frame = FALSE, filename = "was-wirklich-passiert.gif")

df_all <- rbind(df1, df2) %>% 
    mutate(subplot = factor(subplot, 
                            levels = c("Was die Union zu tun glaubt",
                                       "Was wirklich passiert")))

(p3 <- ggplot(df_all, aes(x0 = 0, y0 = 0, start = ymin, end = ymax, 
                       r0 = .5, r = 1, fill = color, frame = frame)) + 
        geom_arc_bar(col = "white", size = 2) + 
        geom_arc_bar(data = df3, aes(r0 = 0.5075, r = 0.9925),
                     col = NA, show.legend = FALSE) +
        scale_fill_identity("", labels = party_labels, breaks = party_colors,
                            guide = "legend") +
        # ggtitle("Was wirklich passiert") +
        facet_wrap(~subplot) +
        coord_fixed() +
        guides(fill = guide_legend(nrow = 1)) +
        theme(axis.title = element_blank(),
              axis.text = element_blank(),
              panel.grid = element_blank(),
              # plot.title = element_text(size = 20, hjust = .5, face = "bold"),
              strip.text = element_text(size = 28, face = "bold"),
              legend.key.size = unit(40, "pt"),
              legend.title = element_blank(),
              legend.text = element_text(size = 20),
              legend.position = "bottom",
              legend.direction = "horizontal"))

gganimate(p3, interval = .2, title_frame = FALSE, 
          ani.width = 1440, ani.height = 480,
          filename = "was-die-union-zu-tun-glaubt-und-was-wirklich-passiert.gif")
