## R Code for the Finance Ministers' radar plot by period ##
## February, 10, 2018 ##

# This is to read in the data and save it as carreira_prop #
radar_periodo <- read.csv2(file = "~/Dropbox/Projetos_R/radar_periodo/radar-5-periodos.csv", header = T, sep = ",")

# Attach the data
attach(radar_periodo)

# This is to set the working directory
# First, save the path to the WD in the object ?ProjectRadarPeriodo?
ProjectRadarPeriodo <- "~/Dropbox/Projetos_R/radar_periodo"
# Then set the WD using this object
setwd(ProjectRadarPeriodo)

# Save workspace
save.image(file = "~/Dropbox/Projetos_R/radar_periodo/ProjectRadarPeriodo.Rdata")

# Load packages
library(ggplot2)
library(reshape2)

# Set data as long
radar_periodo_melted <- melt(radar_periodo, id.vars = "periodo")

# Set the radar coordinates for the plot

coord_radar <- function (theta = "x", start = 0, direction = 1)
{
  theta <- match.arg(theta, c("x", "y"))
  r <- if (theta == "x")
    "y"
  else "x"
  ggproto("CordRadar", CoordPolar, theta = theta, r = r, start = start,
          direction = sign(direction),
          is_linear = function(coord) TRUE)
}

# Create the objects for the plot and its settings

palg <- c("#ffc0cb", "#ffd700", "#008080", "#800080", "#ff7373")

radarplot5 <- ggplot(radar_periodo_melted, aes(x = variable, y = value))

radarplot5 + geom_polygon(aes(group = as.factor(periodo), colour = as.factor(periodo)), fill = NA, size = .8, show.legend = FALSE) +
  geom_line(aes(group = as.factor(periodo), colour = as.factor(periodo)), size = .8) + 
  theme(strip.text.x = element_text(size = rel(1)),
        axis.text.x = element_text(size = rel(1)),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank()) +
  xlab("") + ylab("") +
  guides(color = guide_legend(ncol=1)) +
  coord_radar() + theme_minimal() + 
  scale_color_manual(values = c("#ffc0cb", "#ffd700", "#008080", "#800080", "#ff7373"), name  = "", breaks = c("1", "2", "3", "4", "5"), labels = c("Primeira Republica", "Era Vargas", "Democracia Populista", "Regime Militar", "Democracia Contemporanea"))

# Facets

radarplot5 + geom_polygon(aes(group = as.factor(periodo), colour = as.factor(periodo)), fill = NA, size = .8, show.legend = FALSE) +
  geom_line(aes(group = as.factor(periodo), colour = as.factor(periodo)), size = .8) + 
  theme(strip.text.x = element_text(size = rel(1)),
        axis.text.x = element_text(size = rel(1)),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank()) +
  xlab("") + ylab("") +
  guides(color = guide_legend(ncol=1)) +
  coord_radar() + theme_minimal() + 
  scale_color_manual(values = c("#ffc0cb", "#ffd700", "#008080", "#800080", "#ff7373"), name  = "", breaks = c("1", "2", "3", "4", "5"), labels = c("Primeira Republica", "Era Vargas", "Democracia Populista", "Regime Militar", "Democracia Contemporanea")) +
  facet_wrap(~ periodo)
