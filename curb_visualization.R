library(tidyverse)
library(extrafont)
library(png)
library(grid)
loadfonts(device = "win")
setwd("C:/Users/Alexander/Desktop/Fun Programming/curb_visualization")
curb_ratings <- read.csv('curb_imdb.csv')

theme_set(theme_grey())
theme <- theme_update(axis.title.x = element_text("Franklin Gothic Heavy", size = 20, color = "black"),
                      axis.title.y = element_text("Franklin Gothic Heavy", size = 24, color = 'black'),
                      plot.title = element_text("Franklin Gothic Heavy", size = 48, color = "black"),
                      axis.text = element_text(size = 12, color = "black"),
                      plot.background = element_rect(fill = "#FFC801"),
                      panel.background = element_rect(fill = "#FFC801", colour = "#FFC801",
                                                      size = 2, linetype = "solid"),
                      panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                                      colour = "white"), 
                      panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                                      colour = "white"))

curb_background <- png::readPNG("curb_graphic.png")

curb_graph <- curb_ratings %>% 
  mutate(Season = as.factor(Season)) %>% 
  ggplot(aes(x = Season, y = IMDB.Rating, fill = Season)) +
  annotation_custom(rasterGrob(curb_background, y = unit(.05, "npc"), 
                               width = unit(1,"npc"),
                               height = unit(1,"npc")),
                    -Inf, Inf, -Inf, Inf) +
  geom_vline(aes(xintercept = Season), color = "white") +
  geom_hline(yintercept = c(7, 8, 9, 10), color = "white") +
  geom_violin(show.legend = FALSE,
              draw_quantiles = TRUE,
              color = "gray90") +
  labs(title = "Curb Your Enthusiasm", x = "Season", y = "IMDb Rating") +
  scale_fill_brewer(palette = ("Spectral"))+
  scale_y_continuous(expand = expansion(mult = c(0,0)), limits = c(7,10)) + 
  theme(axis.title.y = element_text(family = "Franklin Gothic Heavy"),
        axis.title.x = element_text(size = 25)) +
  annotate(geom = "label", x = 1, y = 7.35, size = 4, ## label
           label = " S1:E7\nAAMCO\n7.6", 
           family = "Franklin Gothic Heavy",
           hjust = "center",
           lineheight = 0.9,
           color = "#9E0132",
           #label.size = NA,
           fill = "white",) +
annotate(geom = "label", x = 2, y = 9.6, size = 4, ## label
         label = "S2:E7\nThe Doll\n9.4",
         family = "Franklin Gothic Heavy",
         hjust = "center",
         lineheight = 0.9,
         color = "#F46D43",
         #label.size = NA,
         fill = "white")+
annotate(geom = "label", x = 3, y = 9.35, size = 4, ## label
         label = "S3:E10\nThe Grand\nOpening\n9.1",
         family = "Franklin Gothic Heavy",
         hjust = "center",
         lineheight = 0.9,
         color = "#FDAE61",
         #label.size = NA,
         fill = "white") +
annotate(geom = "label", x = 4, y = 7.7, size = 4, ## label
         label = "S4:E1\nMel's Offer\n7.9",
         family = "Franklin Gothic Heavy",
         hjust = "center",
         lineheight = 0.9,
         color = "#FDAE61",
         #label.size = NA,
         fill = "white")  +
annotate(geom = "label", x = 5, y = 9.5, size = 4, ## label
         label = "S5:E8\nThe Ski Lift\n9.2",
         family = "Franklin Gothic Heavy",
         #fontface = "bold",
         hjust = "center",
         lineheight = 0.9,
         color = "goldenrod2",
         #label.size = NA,
         fill = "white") +
annotate(geom = "label", x = 6, y = 7.9, size = 4, ## label
         label = "S6:E4\nThe Lefty Call\n8.2",
         family = "Franklin Gothic Heavy",
         #fontface = "bold",
         hjust = "center",
         lineheight = 0.9,
         color = "darkolivegreen3",
         #label.size = NA,
         fill = "white") +
annotate(geom = "label", x = 7, y = 9.5, size = 4, ## label
         label = "S7:E10\nSeinfeld\n9.2",
         family = "Franklin Gothic Heavy",
         hjust = "center",
         lineheight = 0.9,
         color = "#66C2A5",
         #label.size = NA,
         fill = "white") +
annotate(geom = "label", x = 8, y = 7.7, size = 4, ## label
         label = "S8:E7\nThe Bi-Sexual\n7.9",
         family = "Franklin Gothic Heavy",
         hjust = "center",
         lineheight = 0.9,
         color = "#66C2A5",
         #label.size = NA,
         fill = "white")  +
annotate(geom = "label", x = 9, y = 9.5, size = 4, ## label
         label = "S9:E6\nThe Accidental\nText on Purpose\n9.2",
         family = "Franklin Gothic Heavy",
         hjust = "center",
         lineheight = 0.9,
         color = "#3288BD",
         #label.size = NA,
         fill = "white") +
annotate(geom = "label", x = 10, y = 7.9, size = 4, ## label
         label = "S10:E9\nBeep Panic\n8.1",
         family = "Franklin Gothic Heavy",
         hjust = "center",
         lineheight = 0.9,
         color = "#3288BD",
         #label.size = NA,
         fill = "white")  
#scale_fill_manual(values = curb_palette)
curb_graph


curb_graph + ggsave("curb_graph.png", device = "png", width = 11, height = 8, type = "cairo", dpi = 300)
