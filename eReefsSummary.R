#### EReefs - Hydro and BGC Model Retrieval ####

#### Load Packages ####

# install.packages("devtools")
# devtools::install_github("BarbaraRobson/ereefs")
# install.packages("dplyr")
library(ereefs)
library(dplyr)


#### Load Location Information ####

# Sites with microbial samples
mic_sites = read.csv("sites_coord_dates.csv", header = T)

# just keep coords
sites = mic_sites %>% select(station, lat, long) %>% distinct() %>% na.omit()

# Sites from the AIMS LTMP
AIMS_sites = read.csv("Reef_Sector_Shelf.csv", header=T)

# Sites from Mellin et al 2019 with benthic categorisation
AIMS_locs = read.csv("MRT_survey reefs_bentclust.csv", header = T)
colnames(AIMS_locs)[2] ="REEF_NAME"

# Add predicted benthic category to AIMS LTMP Coordinates
AIMS_sites = AIMS_locs %>% left_join(AIMS_sites[-1], by="REEF_NAME")

# Save AIMS locations
write.csv(AIMS_sites, "AIMS_sites.csv", row.names = F)


#### Download eReefs for Microbial Sites - Entire Time Series ####

# Kd, POC and TSS645 not available

vars = c("Kd_490", "MA_N", "MA_N_gr", "MA_N_pr", "Chl_a_sum", "DIC", "DIN", "DIP", "DOR_C",
         "DOR_N", "DOR_P", "Den_eff", "Den_fl", "DetBL_N", "DetPL_N", "DetR_C", "DetR_N", "DetR_P", "HCO3",
         "Kd_490", "NH4", "NH4_pr", "NO3", "Nfix", "O2_flux", "Oxy_sat", "Oxygen", "PAR", "PH", "PIP", "PIPI",
         "P_Prod", "TC", "TN", "TP", "pco2surf", "tss", "PhyL_Chl", "PhyL_I", "PhyL_N","PhyL_NR", "PhyL_N_gr",
         "PhyL_N_pr", "PhyL_PR", "PhyS_Chl","PhyS_I","PhyS_N","PhyS_NR","PhyS_N_gr", "PhyS_N_pr", "PhyS_PR")

# Download Geoffrey Bay Independently

# 2: Latest release 4km biogeochemical model hindcast (Sept 2010 - Oct 2016)
BGC_Geoffrey = ereefs::get_ereefs_ts(var_names = vars, 
                                     location_latlon = c(-19.18, 146.88)) 
# 3: Latest release 4km biogeochemical model near real time (Oct 2016 - pres.)
BGC_GeoffreyLatest = ereefs::get_ereefs_ts(var_names = vars, 
                                           location_latlon = c(-19.18, 146.88), 
                                           start_date = c(2016,11,01), end_date = c(2018,10,31))

# 1: Latest release 4km grid hydrodynamic model (Sept 2010-pres.)
hydro_Geoffrey = ereefs::get_ereefs_ts(var_names = c("salt", "temp"),
                                       location_latlon = c(-19.18, 146.88))
# 1: Latest release 4km grid hydrodynamic model (Sept 2010-pres.)
hydro_GeoffreyLatest = ereefs::get_ereefs_ts(var_names = c("salt", "temp"),
                                             location_latlon = c(-19.18, 146.88),
                                             start_date = c(2016,11,01), end_date = c(2018,10,31))
# Combine outputs into entrire time series
hydro_Geoffrey = bind_rows(hydro_Geoffrey,hydro_GeoffreyLatest)
BGC_Geoffrey = bind_rows(BGC_Geoffrey,BGC_GeoffreyLatest)


# DOwnload the remainder of the microbial sites

# NB We split up the download to ensure it works and doesn't just 
# spit out an error after an hour of downloading

#### BGC DOwnload ####

# Selection 2 
BGCHind = ereefs::get_ereefs_ts(var_names = vars,
                                location_latlon = as.data.frame(sites[,2:3]))
# Selection 3 
BGCLatest = ereefs::get_ereefs_ts(var_names = vars, 
                                  location_latlon = as.data.frame(sites[,2:3]), 
                                  start_date = c(2016,11,01), end_date = c(2018,10,31))

#### Hydro Download ####

# Selection 1 
hydro = ereefs::get_ereefs_ts(var_names = c("salt", "temp"),
                              location_latlon = as.data.frame(sites[,2:3]), 
                              end_date = c(2018,10,31))
hydrolatest = ereefs::get_ereefs_ts(var_names = c("salt", "temp"),
                                    location_latlon = as.data.frame(sites[,2:3]), 
                                    start_date = c(2016,11,01), end_date = c(2017,01,31))
hydrolatest2 = ereefs::get_ereefs_ts(var_names = c("salt", "temp"),
                                     location_latlon = as.data.frame(sites[,2:3]), 
                                     start_date = c(2017,03,01), end_date = c(2017,03,18))
hydrolatest3 = ereefs::get_ereefs_ts(var_names = c("salt", "temp"),
                                     location_latlon = as.data.frame(sites[,2:3]), 
                                     start_date = c(2017,02,01), end_date = c(2017,2,28))
hydrolatest4 = ereefs::get_ereefs_ts(var_names = c("salt", "temp"),
                                     location_latlon = as.data.frame(sites[,2:3]), 
                                     start_date = c(2017,08,01), end_date = c(2017,10,31))

#### Create Master BGC Dataframe ####

# Add geoffrey
BGCHind$`Geoffrey Bay` = BGC_Geoffrey
BGCLatest$`Geoffrey Bay` = BGC_GeoffreyLatest
hydro$`Geoffrey Bay` = hydro_Geoffrey

# Name each list item
names(BGCHind) = sites$station; names(BGCLatest) = sites$station; names(hydro) = sites$station

#Convert list to dataframe
BGCHind_Long = do.call("rbind", BGCHind)
BGCHind_Long$Station = rep(sites$station, each = length(BGCHind[[1]]$date))
BGCLatest_Long = do.call("rbind", BGCLatest)
BGCLatest_Long$Station = rep(sites$station, each = length(BGCLatest[[1]]$date))

# Create master BGC data frame for 4km resolution
BGC = bind_rows(BGCHind_Long, BGCLatest_Long) %>% arrange(Station, date) %>% select(53, 1:52)


#### Create Master Hydro Dataframe ####

Hydro_Long = do.call("rbind", hydro)

Hydro_Long$Station = row.names(Hydro_Long)
namesies = unlist(lapply(strsplit(row.names(Hydro_Long), ".", fixed = T), `[[`, 1))
Hydro_Long$Station = namesies
hydrolatest_long = do.call("rbind", hydrolatest)
hydrolatest_long$Station = rep(sites$station, each = length(hydrolatest[[1]]$date))
hydrolatest2_long = do.call("rbind", hydrolatest2)
hydrolatest2_long$Station = rep(sites$station, each = length(hydrolatest2[[1]]$date))
hydrolatest3_long = do.call("rbind", hydrolatest3)
hydrolatest3_long$Station = rep(sites$station, each = length(hydrolatest3[[1]]$date))
hydrolatest4_long = do.call("rbind", hydrolatest4)
hydrolatest4_long$Station = rep(sites$station, each = length(hydrolatest4[[1]]$date))


# Create master BGC data frame for 4km resolution
HYD = bind_rows(Hydro_Long, hydrolatest_long, hydrolatest2_long, hydrolatest3_long, hydrolatest4_long) %>%
  arrange(Station, date) %>%
  dplyr::select(4,1:3)

# Write as csv
write.csv(BGC, "BGC.csv", row.names = F)
write.csv(HYD, "HYD.csv", row.names = F)
write.csv(sites, "sites.csv", row.names = F)




#### Create Summaries for 3day, 1 week, 2 weeks, 1 month before Sampling ####

# Create dataframe with site location and Dates
mic_sites$date.1 = as.Date(mic_sites$date.1, format = "%d/%m/%Y")
sites_dates = mic_sites %>% select(station, date.1, lat, long) %>% distinct() %>% na.omit()
colnames(sites_dates)[2] = "date"
colnames(sites_dates)[1] = "Station"
colnames(mic_sites)[2] = "Station"

# Function to retrieve data for the preceding window
selectdates = function(df, ndays) {
  df = df %>%  mutate(Start = date-(ndays-1)) 
  colnames(df)[5] = sprintf("Start_%sd", 3)
  BGCselectDates= df[rep(seq.int(1,nrow(df)), each=ndays),] 
  BGCselectDates$SampleDate = BGCselectDates$date
  for (i in seq(1, length(df[,1])*ndays, by=ndays)){
    BGCselectDates$date[i:(i+(ndays-1))] = seq.Date(from=BGCselectDates[5][i,], to = BGCselectDates$date[i], by = 1)
  }
  return(BGCselectDates)
}

# BGC Data
BGCselect_3d = selectdates(sites_dates, 3)
BGCselect_1w = selectdates(sites_dates, 7)
BGCselect_2w = selectdates(sites_dates, 14)
BGCselect_1m = selectdates(sites_dates, 30)

# Merge Data
BGCselect_3d = merge(BGC, BGCselect_3d, by=1:2,all.y = T)
BGCselect_1w = merge(BGC, BGCselect_1w, by=1:2,all.y = T)
BGCselect_2w = merge(BGC, BGCselect_2w, by=1:2,all.y = T)
BGCselect_1m = merge(BGC, BGCselect_1m, by=1:2,all.y = T)

# HYD Data using dates retrieved above
HYDselect_3d = merge(HYD, BGCselect_3d[c(1,2,57)], by=1:2,all.y = T)
HYDselect_1w = merge(HYD, BGCselect_1w[c(1,2,57)], by=1:2,all.y = T)
HYDselect_2w = merge(HYD, BGCselect_2w[c(1,2,57)], by=1:2,all.y = T)
HYDselect_1m = merge(HYD, BGCselect_1m[c(1,2,57)], by=1:2,all.y = T)

# now i compute mean and std error
install.packages("plotrix")
library(plotrix)
BGC3d = BGCselect_3d %>% ungroup() %>% group_by(Station, SampleDate) %>% 
  select(-lat, -long, -Start_3d, -date) %>% summarise_at(3:52,funs(mean, se=std.error),na.rm =T)
BGC1w = BGCselect_1w %>% ungroup() %>% group_by(Station, SampleDate) %>% 
  select(-lat, -long, -Start_3d, -date) %>% summarise_at(3:52,funs(mean, se=std.error),na.rm =T)
BGC2w = BGCselect_2w %>% ungroup() %>% group_by(Station, SampleDate) %>% 
  select(-lat, -long, -Start_3d, -date) %>% summarise_at(3:52,funs(mean, se=std.error),na.rm =T)
BGC1m = BGCselect_1m %>% ungroup() %>% group_by(Station, SampleDate) %>% 
  select(-lat, -long, -Start_3d, -date) %>% summarise_at(3:52,funs(mean, se=std.error),na.rm =T)

HYD3d = HYDselect_3d %>% ungroup() %>% group_by(Station, SampleDate) %>% 
  select(-date) %>% summarise_at(2:3,funs(mean, se=std.error),na.rm =T)
HYD1w = HYDselect_1w %>% ungroup() %>% group_by(Station, SampleDate) %>% 
  select(-date) %>% summarise_at(2:3,funs(mean, se=std.error),na.rm =T)
HYD2w = HYDselect_2w %>% ungroup() %>% group_by(Station, SampleDate) %>% 
  select(-date) %>% summarise_at(2:3,funs(mean, se=std.error),na.rm =T)
HYD1m = HYDselect_1m %>% ungroup() %>% group_by(Station, SampleDate) %>% 
  select(-date) %>% summarise_at(2:3,funs(mean, se=std.error),na.rm =T)

# COmbine HYD and BGC to give 1 dataframe
eReefsSum3d = inner_join(HYD3d, BGC3d)
eReefsSum1w = inner_join(HYD1w, BGC1w)
eReefsSum2w = inner_join(HYD2w, BGC2w)
eReefsSum1m = inner_join(HYD1m, BGC1m)

# Write files
write.csv(eReefsSum3d, "eReefsSum3d.csv", row.names = F)
write.csv(eReefsSum1w, "eReefsSum1w.csv", row.names = F)
write.csv(eReefsSum2w, "eReefsSum2w.csv", row.names = F)
write.csv(eReefsSum1m, "eReefsSum1m.csv", row.names = F)


#### Download 1km Reoslution for AIMS Sites ####

# Selection 12 
BGC_AIMS = ereefs::get_ereefs_ts(var_names = vars,
                                      location_latlon = as.data.frame(AIMS_sites[,c(4,3)]))

# Selection 6
hydro_AIMS = ereefs::get_ereefs_ts(var_names = c("salt", "temp"),
                                   location_latlon = as.data.frame(AIMS_sites[,c(4,3)]),
                                   end_date = c(2018,10,31))

#### Download 1km Reoslution for MIC Sites ####

# Selection 12 
BGC = ereefs::get_ereefs_ts(var_names = vars,
                                location_latlon = as.data.frame(sites[,2:3]),
                                end_date = c(2018,10,31))

# Selection 6
HYD = ereefs::get_ereefs_ts(var_names = c("salt", "temp"),
                                   location_latlon = as.data.frame(sites[,2:3]),
                                   end_date = c(2018,10,31))

#### Seasonal Averages for AIMS Sites ####

#remove duplicates
AIMS_sites2 = AIMS_sites[-which(duplicated(AIMS_sites[2:4])),]
remove = which(duplicated(AIMS_sites[2:4]))

hydro_AIMS = hydro_AIMS[-remove]
BGC_AIMS = BGCHind_AIMS2[-remove]

names(hydro_AIMS) = AIMS_sites2$REEF_NAME; names(BGC_AIMS) = AIMS_sites2$REEF_NAME;

# Create Data Frames and Name sites
hydro_AIMSLong = do.call("rbind", hydro_AIMS2)
hydro_AIMSLong$REEF_NAME = rep(AIMS_sites2$REEF_NAME, each = length(hydro_AIMS2[[1]]$date))

# Create Data Frames and Name sites
BGC_AIMSLong = do.call("rbind", BGC_AIMS)
BGC_AIMSLong$REEF_NAME = rep(AIMS_sites2$REEF_NAME, each = length(BGC_AIMS[[1]]$date))

BGC_AIMS = BGC_AIMS %>% 
  arrange(REEF_NAME, date) %>% select(53, 1:52)
HYD_AIMS = dplyr::select(hydro_AIMSLong, 4, 1:3)

# AIMS Summaries

# These summaries are for entire year year NB WE avoided the 2016, 2017 Bleaching events

BGC_AIMSsum = BGC_AIMS %>% filter(date > "2011-02-01" & date < "2015-12-31") %>% group_by(REEF_NAME) %>% select(-date) %>% summarise_all(funs(mean, sd),na.rm =T)
Hyd_AIMSsum = HYD_AIMS %>% filter(date > "2011-02-01" & date < "2015-12-31") %>% group_by(REEF_NAME) %>% select(-date) %>% summarise_all(funs(mean, sd),na.rm =T)

# AIMS MOnth Summary BGC ----
BGC_AIMSsumMonth= BGC_AIMS %>% 
  mutate(month = months(date, abbreviate=T)) %>%
  filter(date > "2011-02-01" & date < "2015-12-31") %>% 
  group_by(REEF_NAME, month) %>% select(-date, -Kd_490.1) %>% summarise_all(funs(mean, sd),na.rm =T) %>%
  inner_join(AIMS_sites2[,-1], by = "REEF_NAME") %>%
  select(1, X:SHELF, 2:102)
# AIMS Wet-Dry Season Summary BGC ----
BGC_AIMSsumSeason= BGC_AIMS %>% 
  mutate(month = months(date, abbreviate=T),
         Season = ifelse(month %in% c("Dec", "Jan", "Feb"), "Wet", 
                         ifelse(month %in% c("Jun", "Jul", "Aug"), "Dry", NA))) %>%
  filter(date > "2011-02-01" & date < "2015-12-31" & Season %in% c("Wet", "Dry")) %>% 
  group_by(REEF_NAME, Season) %>% select(-date, -Kd_490.1, -month) %>% summarise_all(funs(mean, sd),na.rm =T) %>%
  inner_join(AIMS_sites2[,-1], by = "REEF_NAME") %>%
  select(1, X:SHELF, 2:102)

# AIMS MOnth Summary HYD ----
HYD_AIMSsumMonth= HYD_AIMS %>% 
  mutate(month = months(date, abbreviate=T)) %>%
  filter(date > "2011-02-01" & date < "2015-12-31") %>% 
  group_by(REEF_NAME, month) %>% select(-date) %>% summarise_all(funs(mean, sd),na.rm =T) %>%
  inner_join(AIMS_sites2[,-1], by = "REEF_NAME") %>%
  select(1, X:SHELF, 2:6)
# AIMS Wet-Dry Season Summary HYD ----
HYD_AIMSsumSeason= HYD_AIMS %>% 
  mutate(month = months(date, abbreviate=T),
         Season = ifelse(month %in% c("Dec", "Jan", "Feb"), "Wet", 
                         ifelse(month %in% c("Jun", "Jul", "Aug"), "Dry", NA))) %>%
  filter(date > "2011-02-01" & date < "2015-12-31" & Season %in% c("Wet", "Dry")) %>% 
  group_by(REEF_NAME, Season) %>% select(-date, -month) %>% summarise_all(funs(mean, sd),na.rm =T) %>%
  inner_join(AIMS_sites2[,-1], by = "REEF_NAME") %>%
  select(1, X:SHELF, 2:6)

# Combine HYD and BGC Variables
eReefsSumMonths_AIMS = inner_join(HYD_AIMSsumMonth, BGC_AIMSsumMonth, by = colnames(BGC_AIMSsumMonth)[1:7])
eReefsSumSeason_AIMS = inner_join(HYD_AIMSsumSeason, BGC_AIMSsumSeason, by = colnames(BGC_AIMSsumSeason)[1:7])

# write to file
write.csv(eReefsSumMonths_AIMS, "eReefSumMonths_AIMS.csv", row.names = F)
write.csv(eReefsSumSeason_AIMS, "eReefSumSeason_AIMS.csv", row.names = F)



# Mic Sites Summaries


# Create Data Frames and Name sites
HYDLong = do.call("rbind", HYD)
HYDLong$Station = rep(sites$station, each = length(HYD[[1]]$date))

# Create Data Frames and Name sites
BGCLong = do.call("rbind", BGC)
BGCLong$Station = rep(sites$station, each = length(BGC[[1]]$date))

BGC = BGCLong %>% 
  arrange(station, date) %>% select(53, 1:52)
HYD = dplyr::select(HYDLong, 4, 1:3)


colnames(sites)[1] = "Station"
# Microbial Month Summary BGC ----
BGC_sumMonth= BGC %>% 
  mutate(month = months(date, abbreviate=T)) %>%
  filter(date > "2011-02-01" & date < "2015-12-31") %>% 
  group_by(Station, month) %>% select(-date, -Kd_490.1) %>% summarise_all(funs(mean, sd),na.rm =T) %>%
  inner_join(sites, by = "Station") %>%
  select(1, lat:long, 2:102)
# Microbial Wet-Dry Season Summary BGC ----
BGC_sumSeason= BGC %>% 
  mutate(month = months(date, abbreviate=T),
         Season = ifelse(month %in% c("Dec", "Jan", "Feb"), "Wet", 
                         ifelse(month %in% c("Jun", "Jul", "Aug"), "Dry", NA))) %>%
  filter(date > "2011-02-01" & date < "2015-12-31" & Season %in% c("Wet", "Dry")) %>% 
  group_by(Station, Season) %>% select(-date, -Kd_490.1, -month) %>% summarise_all(funs(mean, sd),na.rm =T) %>%
  inner_join(sites, by = "Station") %>%
  select(1, lat:long, 2:102)
# Microbial Month Summary HYD ----
HYD_sumMonth= HYD %>% 
  mutate(month = months(date, abbreviate=T)) %>%
  filter(date > "2011-02-01" & date < "2015-12-31") %>% 
  group_by(Station, month) %>% select(-date) %>% summarise_all(funs(mean, sd),na.rm =T) %>%
  inner_join(sites, by = "Station") %>%
  select(1, lat:long, 2:6)
# Microbial Wet_Dry Season Summary HYD ----
HYD_sumSeason= HYD %>% 
  mutate(month = months(date, abbreviate=T),
         Season = ifelse(month %in% c("Dec", "Jan", "Feb"), "Wet", 
                         ifelse(month %in% c("Jun", "Jul", "Aug"), "Dry", NA))) %>%
  filter(date > "2011-02-01" & date < "2015-12-31" & Season %in% c("Wet", "Dry")) %>% 
  group_by(Station, Season) %>% select(-date, -month) %>% summarise_all(funs(mean, sd),na.rm =T) %>%
  inner_join(sites, by = "Station") %>%
  select(1, lat:long, 2:6)

eReefsSumMonths = inner_join(HYD_sumMonth, BGC_sumMonth, by = colnames(BGC_sumMonth)[1:4])
eReefsSumSeason = inner_join(HYD_sumSeason, BGC_sumSeason, by = colnames(BGC_sumSeason)[1:4])

# write to file
write.csv(eReefsSumMonths, "eReefSumMonths_MIC.csv", row.names = F)
write.csv(eReefsSumSeason, "eReefSumSeason_MIC.csv", row.names = F)


 

