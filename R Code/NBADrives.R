library(tidyverse)


drives <- read.csv("NBA_Drives.csv") # Data from NBA.com stats


# Create the simple per drive statistics
# Filter by >= 200 drives to get players with a sufficient sample of drives

drives <- drives %>%
  group_by(PLAYER) %>%
  mutate(FTA_Drive23 = FTA / DRIVES,
         Fouls_Drive23 = PF / DRIVES) %>%
  filter(DRIVES >= 200) %>%
  arrange(-FTA_Drive)

# Create the average values for fouls and fta per drive in the aggregate

fta_drive <- sum(drives$FTA) / sum(drives$DRIVES)
pf_drive <- sum(drives$PF) / sum(drives$DRIVES)

# Create FTA/Fouls per Drive Over Expected for 2023 
drives <- drives %>%
  mutate(FTA_DriveOE23 = FTA_Drive - fta_drive,
         Fouls_DriveOE23 = Fouls_Drive - pf_drive)

drives <- drives[-c(24:27)]

drives %>%
  select(PLAYER, FTA_DriveOE23) %>%
  arrange(-FTA_DriveOE23) %>%
  top_n(20) %>%
  print(n = 20)




drives1 <- read.csv("NBA_drives22.csv") # 2022 Data from NBA.com 

#Repeat process for 2022 data

drives1 <- drives1 %>%
  group_by(PLAYER) %>%
  mutate(FTA_Drive22 = FTA / DRIVES, 
         Fouls_Drive22 = PF / DRIVES) %>%
  filter(DRIVES >= 200) %>%
  arrange(-FTA_Drive)

fta_drive22 <- sum(drives1$FTA) / sum(drives1$DRIVES)
pf_drive22 <- sum(drives1$PF) / sum(drives1$DRIVES)


drives1 <- drives1 %>%
  mutate(FTA_DriveOE22 = FTA_Drive22 - fta_drive22, 
         Fouls_DriveOE22 = Fouls_Drive22 - pf_drive22)

drives1 <- drives1[-c(24:25)]

drives1 %>%
  select(PLAYER, FTA_DriveOE22) %>%
  arrange(-FTA_DriveOE22) %>%
  top_n(20) %>%
  print(n = 20)


# Join datasets from 22 and 23

drives2 <- inner_join(drives1,drives, by = "PLAYER")

# Create overall statistics for 22-23

drives2 <- drives2 %>%
  mutate(FTA_DriveOE_22_23 = ((FTA_DriveOE23 * DRIVES.y) + (FTA_DriveOE22 * DRIVES.x)) / (DRIVES.y + DRIVES.x),
         Fouls_DriveOE_22_23 = ((Fouls_DriveOE23 * DRIVES.y) + (Fouls_DriveOE22 * DRIVES.x)) / (DRIVES.y + DRIVES.x),
         Total_FTADriveOE_23 = (FTA_DriveOE23 * DRIVES.y),
         Total_FoulsDriveOE_23 = (Fouls_DriveOE23 * DRIVES.y),
         Total_FTADriveOE_22 = (FTA_DriveOE22 * DRIVES.x),
         Total_FoulsDriveOE_23 = (Fouls_DriveOE22 * DRIVES.x),
         Total_FTA_DriveOE_22_23 = (FTA_DriveOE_22_23 * (DRIVES.x + DRIVES.y)),
         Total_Fouls_DriveOE_22_23 = (Fouls_DriveOE_22_23 * (DRIVES.x + DRIVES.y)))

library(ggpubr)
library(ggthemes)

# Graph of the YEAR TO YEAR Correlation of FDOE
drives3 %>%
  ggscatter(x = "Fouls_DriveOE22", y = "Fouls_DriveOE23") +
  stat_cor(method = "pearson") +
  geom_smooth(method = lm)+
  theme_clean()+
  labs(x = "Fouls per Drive Over Expected in 2022", 
       y = "Fouls per Drive Over Expected in 2023",
       title = "Fouls per Drive Over Expected in 2022-23 vs. 2021-22",
       subtitle = "Minimum 200 Drives in Both Seasons", 
       caption = "By Rohan Patel, @rpat_57") + 
  theme(plot.title = element_text(size = 22, hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(size = 10, hjust = 0.5)) 
ggsave("fdoegraph.png")

options(digits = 2)



p <- drives2 %>%
  select(PLAYER, Total_FTA_DriveOE_22_23) %>%
  arrange(-Total_FTA_DriveOE_22_23) %>%
  top_n(20) %>%
  print(n = 20) 

# Trying to look at how height and weight affect foul calls

bios <- read.csv("NBA_Bios.csv")
bios <- bios[-c(6:20)]

drives3 <- drives2 %>% # Select Important Columns, Rename the ones with.X then join the bios then separate the height column
  select(PLAYER,
         DRIVES.x,
         FTA_Drive22,
         Fouls_Drive22,
         FTA_DriveOE22,
         Fouls_DriveOE22,
         DRIVES.y,
         FTA_Drive23,
         Fouls_Drive23,
         FTA_DriveOE23,
         Fouls_DriveOE23,
         FTA_DriveOE_22_23,
         Fouls_DriveOE_22_23,
         Total_FTADriveOE_22,
         Total_FTADriveOE_23,
         Total_FTA_DriveOE_22_23)
colnames(drives3)[2] = "Drives_22" 
colnames(drives3)[7] = "Drives_23"

drives3 <- drives3 %>%
  left_join(bios, by = "PLAYER")

drives3 <- drives3 %>%
  separate(HEIGHT,c('feet', 'inches'), sep = '-', convert = TRUE, remove = FALSE) %>%
  mutate(Height = 12*feet + inches) %>%
  select(-inches)

# Correlations and Modeling to see the correlations between Height and Weight
drives3 %>%
  ggscatter(x = "Height", y = "FTA_DriveOE_22_23") +
  stat_cor(method = "pearson") +
  geom_smooth(method = "lm")

drives3 %>%
  ggscatter(x = "WEIGHT", y = "FTA_DriveOE_22_23") +
  stat_cor(method = "pearson") +
  geom_smooth(method = "lm")

model <- glm(FTA_DriveOE_22_23 ~ Height + WEIGHT, data = drives3)

summary(model) #Nothing particularly sufficient but may require further explanation

# Create table for FDOE

drives4 <- drives3 %>%
  mutate(Total_FoulsOE23 = Fouls_DriveOE23 * Drives_23) %>%
  mutate(Fouls_DriveOE23 = round(Fouls_DriveOE23,3),
         Total_FoulsOE23 = round(Total_FoulsOE23,3)) %>%
  arrange(-Total_FoulsOE23) %>%
  mutate(rank = row_number())


drivesgt <- drives4 %>%
  select(PLAYER,HEIGHT,TEAM, Drives_23,Fouls_DriveOE23, Total_FoulsOE23) %>%
  arrange(-Fouls_DriveOE23) %>%
  gt(groupname_col = drives4$PLAYER) %>%
  cols_align(align = "center") %>%
  cols_label(Total_FoulsOE23 = "Total Fouls Over Expected",
             PLAYER = "Player", 
             Drives_23 = "Drives",
             Fouls_DriveOE23 = "Fouls per Drive Over Expected") %>%
  gtExtras::gt_theme_pff() %>%
  gtExtras::gt_hulk_col_numeric(Total_FoulsOE23) %>%
  gtExtras::gt_color_rows(Fouls_DriveOE23, 
                          palette = c("blue","white","red"))
  
  
gtsave(drivesgt23,"23foulsoe.png")


drives <- left_join(drives,bios, by = "PLAYER")

drivesgt231 <- drives %>%
  mutate(Total_FoulsOE23 = Fouls_DriveOE23 * DRIVES,
         Total_FoulsOE23 = round(Total_FoulsOE23,2),
         Fouls_DriveOE23 = round(Fouls_DriveOE23,3)) %>%
  select(PLAYER,HEIGHT,TEAM.x, DRIVES,Fouls_DriveOE23, Total_FoulsOE23) %>%
  arrange(-Fouls_DriveOE23) %>%
  gt(groupname_col = drives4$PLAYER) %>%
  cols_align(align = "center") %>%
  cols_label(Total_FoulsOE23 = "Total Fouls Over Expected",
             PLAYER = "Player", 
             DRIVES = "Drives",
             Fouls_DriveOE23 = "Fouls per Drive Over Expected",
             TEAM.x = "Team") %>%
  gtExtras::gt_theme_pff() %>%
  gtExtras::gt_hulk_col_numeric(Total_FoulsOE23) %>%
  gtExtras::gt_color_rows(Fouls_DriveOE23, 
                          palette = c("blue","white","red"))

gtsave(drivesgt231, "drives23real.png")

# Trying to create a metric which looks at what players overperform their height in terms of foul calls

heights <- drives %>%
  group_by(HEIGHT) %>%
  summarise(expfoul_drives = sum(PF)/sum(DRIVES))

drives <- left_join(drives,heights,by="HEIGHT")

drives %>% 
  mutate(fouls_oe_drive_height = Fouls_Drive23 - expfoul_drives) %>%
  select(PLAYER, HEIGHT, fouls_oe_drive_height) %>%
  arrange(desc(fouls_oe_drive_height))



logos <- read.csv("logos4.csv")





drives_teams <- drives %>%
  group_by(TEAM.x) %>%
  select(PF, DRIVES, TEAM.x) %>%
  summarize(avgfdoe = (sum(PF) / sum(DRIVES)) - pf_drive,
            Drives = sum(DRIVES)) %>%
  left_join(logos,by = c("TEAM.x" = "team")) 

  
  
# Graph for teams in 23 

ggplot(drives_teams, aes(x = Drives, y = avgfdoe)) +
  geom_image(aes(image = logo), size = 0.1) +
  theme_clean()+
  scale_x_continuous(breaks = scales::pretty_breaks(n =8)) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 4)) + 
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_vline(xintercept = mean(drives_teams$Drives), linetype ="dashed") +
  labs(x = "Drives",
       y = "Fouls per Drive Over Expected",
       title = "Fouls per Drive Over Expected vs Drives",
       subtitle = "NBA Teams in 2022-23",
       caption = "By Rohan Patel | @rpat_57") +
  theme(plot.title = element_text(size = 22, hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(size = 10, hjust = 0.5)) 
  
 
ggsave('fdoeteams.png')

 view(drives)
 
 
 
drives <- drives %>%
  separate(HEIGHT,c('feet', 'inches'), sep = '-', convert = TRUE, remove = FALSE) %>%
  mutate(Height = 12*feet + inches) %>%
  select(-inches,-feet)
 
 
 
view(drives)


fdoe_height <- drives %>%
  group_by(HEIGHT) %>%
  summarize(FDOE_23 = (sum(PF) / sum(DRIVES)) - pf_drive) %>%
  arrange(desc(HEIGHT))