#######################################################################################
############# The script has been originally produced by Peter Ellis  #################
###The dataset utilized are from Johns Hopkins University #############################
#######The script utilizes the "coronavirus" package developed by Rami Krispin#########
###############The script has been modified by Veo Chae################################
#######################################################################################


#--------------- Setup--------------------
devtools::install_github("RamiKrispin/coronavirus")
library(coronavirus)
library(tidyverse)
library(scales)

#package "ggpubr" was added to create a grid for 2 graphs (Edit: Chae)
install.packages("ggpubr")
library(ggpubr)

the_caption = "Source: WHO and many others via Johns Hopkins University and Rami Krispin's coronavirus R package.\nAnalysis by http://freerangestats.info"

#Changed from Top 8 --> Top 10(Edit: Chae)
top_countries <- coronavirus %>%
  filter(type == "confirmed") %>%
  group_by(country) %>%
  summarise(cases = sum(cases)) %>%
  top_n(10, wt = cases)

#---------------------------global total-------------------

#Non China is being assessed as China is the starting point of the pandemic.
first_non_china_d <- coronavirus %>%
  filter(country != "China" & type == "death" & cases > 0) %>%
  arrange(date) %>%
  slice(1) %>%
  pull(date)

#Alteration from Italty --> US(Edit: Chae)
first_US_d <- coronavirus %>%
  filter(country == "US" & type == "death" & cases > 0) %>%
  arrange(date) %>%
  slice(1) %>%
  pull(date)

#grouping in order to create a cumulative worldwide fatality rate over time
d1 <- coronavirus %>%
  group_by(date, type) %>%
  summarise(cases = sum(cases)) %>%
  arrange(date) %>%
  spread(type, cases) %>%
  ungroup() %>%
  mutate(cfr_today = death / confirmed,
         cfr_cumulative_1 = cumsum(death) / cumsum(confirmed))

d1b <- d1 %>%
  filter(date %in% c(first_US_d, first_non_china_d))

ac <- "steelblue"

#Observing the first global date when passing 10,000 and 100,000 cases of confirmed cases.
d1c <- d1 %>%
  mutate(cc = cumsum(confirmed)) %>% 
  summarise(`10000` = min(date[cc > 10000]),
            `100000` = min(date[cc > 100000])) %>%
  gather(variable, date) %>%
  left_join(d1, by = "date") %>%
  mutate(label = paste0(format(as.numeric(variable), big.mark = ",", scientific = FALSE), "\ncases"))

#Assigned the graph with name "GLOBAL" in order to put together 2 graphs at the end (Edit: Chae)
GLOBAL <- d1 %>%
  ggplot(aes(d1,x = date, y = cfr_cumulative_1)) +
  geom_line() +
  scale_y_continuous(label = percent_format(accuracy = 0.1)) +
  expand_limits(y = 0) +
  geom_point(data = d1b, colour = ac, shape = 1, size = 2) +
#added a geom_point to point out when was the date when the 10,000 and 100,000 confirmed cases took place within the graph (Edit: Chae)
  geom_point(data = d1c, color = "black", shape = 1, size = 2)+
  annotate("text", x = first_US_d, 
           y = filter(d1, date == first_US_d)$cfr_cumulative_1 - 0.001, 
           label = "First death in US",
           hjust = 0, size = 2, colour = ac) +
#added annotation so that the date of the First US death will be presented (Edit: Chae)
  annotate("text", x = first_US_d, 
           y = filter(d1, date == first_US_d)$cfr_cumulative_1 - 0.002, 
           label = first_US_d,
           hjust = 0, size = 2, colour = ac) +
  annotate("text", x = first_non_china_d, 
           y = filter(d1, date == first_non_china_d)$cfr_cumulative_1 + 0.001, 
           label = "First death outside China",
           hjust = -.15, size = 2, colour = ac) +
#added annotation so that the date of the First Non-US death will be presented (Edit: Chae)
  annotate("text", x = first_non_china_d, 
           y = filter(d1, date == first_non_china_d)$cfr_cumulative_1, 
           label = first_non_china_d,
           hjust = -.2, size = 2, colour = ac) +
#changed the text color from grey --> black + nudge_y to to 0.003 (Edit: Chae)
  geom_text(data = d1c, aes(label = label), 
            size = 2, colour = "black", 
            hjust = 1, lineheight = 0.9, nudge_y = 0.003) +
  labs(x = "",
       y = "Observed case fatality rate",
#deleted the outdated title and adjusted to the title below for up to date use (Edit: Chae)
       title = "Global case fatality rate of COVID-19")+
#line 75 was added to center the graph Title(Edit: Chae)
  theme(plot.title = element_text(hjust=0.5))

#-----------------Country-specific totals------------------------

#calculating country specific fatality rate and merge with the Top 10 countries list
d2 <- coronavirus %>%
  group_by(date, country, type) %>%
  summarise(cases = sum(cases)) %>%
  group_by(date, country) %>%
  spread(type, cases) %>%
  arrange(date) %>%
  group_by(country) %>%
  mutate(cfr_cumulative = cumsum(death) / cumsum(confirmed)) %>%
  filter(!is.na(cfr_cumulative)) %>%
  ungroup() %>%
  inner_join(top_countries, by = "country") 


#Assigned the graph with name "COUNTRY" in order to place it in grid at the end (Edit: Chae)
COUNTRY <- d2 %>%
  ggplot(aes(x = date, y = cfr_cumulative, colour = country)) +
  geom_line() +
  geom_text(data = filter(d2, date == max(date)), aes(label = country), 
            hjust = 0, check_overlap = FALSE, size = 3) +
  scale_y_continuous(label = percent_format(accuracy = 1), limits = c(0, 0.16)) +
#Pallete Color was Changed from Set 2 to Set 3 as Set 2 only included 8 colors
#Top 8 countries were changed to Top 10 in the beginning, thus required a new pallete (Edit: Chae)
  scale_colour_brewer(palette = "Set3") +
  expand_limits(x = max(d2$date) + 10) +
  labs(caption = the_caption,
       x = "",
#deleted the y axis for better reading when placed on grid at the end (Edit: Chae)
       y = "",
#The title was adjusted so that it can be used with the up to date information (Edit: Chae)
       title = "Country-specific case fatality rate of COVID-19") +
#line 104 was added to center the graph Title (Edit: Chae)
  theme(plot.title = element_text(hjust=0.5))+
#Caption Size was adjusted (Edit: Chae)
  theme(plot.caption = element_text(size = 4))+
#legend was added as the marking on the graph itself was hard to read (Edit: Chae)
  theme(legend.position = "bottom")+
  theme(legend.title = element_text( size=5), legend.text=element_text(size=5))



#------------creating grid for the 2 graphs ---------------

#placed the "GLOBAL" and "COUNTRY" graphs into a grid so that it is easier to view (Edit: Chae)
ggarrange(GLOBAL,COUNTRY)

output$global_fatal <- renderLeaflet(GLOBAL)
output$global_Top_10 <- renderLeaflet(COUNTRY)
