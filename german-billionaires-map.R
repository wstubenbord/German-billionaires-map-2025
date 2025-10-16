library(tidyverse)
library(rnaturalearth)    # Provides map shape files
library(ggthemes)         # Provides theme_map()
library(ggtext)           # Allows for italicizing the data source name

# --- Read data --- #
geocoded <- read_csv("german_billionaires_city-level.csv")

# --- Map elements from rnaturalearth  --- #
de <- ne_countries(country = "Germany", returnclass = "sf", scale = "large")
de_states <- ne_states(country = "Germany", returnclass = "sf")

# --- Labels for Top 5 Cities by # of Billionaires --- #
labels <- geocoded %>% 
  arrange(desc(n)) %>%
  slice_head(n = 5)

# --- Plot --- #
p <- ggplot(de) +
  geom_sf(fill = "grey95", color = "grey70", linewidth = 0.2) +
  geom_sf(data = de_states, fill = NA, color = "grey60", linewidth = 0.3) +
  geom_point(data = geocoded,
             aes(x = lon, y = lat, size = total_worth, fill = n), 
             shape = 21, color = "black", alpha = 0.60) +
  scale_fill_viridis_c(name = "Number of Billionaires",
                       breaks = c(5, 10, 15),
                       limits = c(0, max(geocoded$n)),
                       option = "viridis") +
  scale_size_area(name = "Aggregate Wealth (USD)", 
                  max_size=22,
                  labels = scales::label_dollar(suffix = "B", accuracy = 1),
                  guide = guide_legend(reverse = TRUE)) +
  theme_map(base_size = 20, base_family = "Roboto Condensed")  +
  theme(plot.title = element_text(family = "Roboto Condensed", 
                                  face = "bold", 
                                  size = 35),
        plot.subtitle = element_text(family = "Roboto Condensed Light", 
                                     face = "plain",
                                     size = 22),
        plot.caption = element_markdown(family = "Roboto Condensed",
                                        face = "plain",
                                        size = 14,
                                        hjust = 0),
        legend.title = element_text(family = "Roboto Condensed Light", 
                                    face = "plain",
                                    size = 18),
        legend.text = element_text(family = "Roboto Condensed Light", 
                                   face = "plain",
                                   size = 16),
        legend.position = "right",
        legend.box = "vertical",
        legend.justification = c("left", "center"),
        legend.box.just = "left",
        legend.spacing.y = unit(0.8, 'cm'),
        legend.key.height = unit(20, "pt"),
        legend.key.width  = unit(10, "pt"),
        plot.margin = margin(30, 10, 30, 0),
        plot.caption.position = "plot") +
  labs(title = "Distribution of Concentrated Wealth in Germany",
       subtitle = "By residence location of 157 German billionaires (2025)",
       caption = "Map by Wesley Stubenbord; Data: <i>Forbes</i>") +
  ggrepel::geom_text_repel(data = labels,
                           aes(lon, lat, label = city),
                           size = 8,
                           family = "Roboto Condensed",
                           segment.size  = 0)
  
ggsave("map_wealth_concentration_germany.png", 
       plot = p, 
       bg = 'white', 
       width = 11, 
       height = 9.5, 
       dpi = 300)
