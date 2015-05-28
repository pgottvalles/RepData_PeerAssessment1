
setwd("/home/patrick/Documents/R_Work_Space/Reproductible Reasearch")
furl <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2"
fname <- "StormData.csv.bz2"


library(plyr)
library(lubridate)

download.file(furl, destfile = fname, method = "curl")
ds <- read.csv(bzfile(fname),stringsAsFactors=FALSE )

ds$BGN_YEAR <- year(as.Date(ds$BGN_DATE,"%m/%d/%Y"))

ds$EVTYPE_CAT[grepl("*FLOOD*|*TIDES*|*HEAVY RAIN*|*HIGH WAVES*|TORRENTIAL RAIN|*RAINFALL*|*SHOWER|*high surf*|TSUNAMI|*PRECIPITATION*|*SWELLS",ignore.case = TRUE,ds$EVTYPE)] <- "Flood"
ds$EVTYPE_CAT[grepl("Snow*|Wint*|Freez*|Glaze*|Freez*|Glaze*|BLIZZARD*|Chill*|Cold*|*Ice*|*Low temp*|Frost*|Icy*|*Cool*",ignore.case = TRUE,ds$EVTYPE)] <- "Cold"
ds$EVTYPE_CAT[grepl("HAIL*",ignore.case = TRUE,ds$EVTYPE)] <- "Hail"
ds$EVTYPE_CAT[grepl("HURRICANE*|Wind*|*TYPHOON*|*THUNDER*|*STORM*|TSTM*|BLOWING DUST*|LIGHTNING*|*WALL CLOUD*|*FUNNEL*|WND",ignore.case = TRUE,ds$EVTYPE)] <- "Storm"
ds$EVTYPE_CAT[grepl("TORNADO*|GUSTNADO*",ignore.case = TRUE,ds$EVTYPE)] <- "Tornado"
ds$EVTYPE_CAT[grepl("*Heat*|*High tem*|*Fire*|Dry*|BELOW NORMAL PRECIPITATION|*DUST|WARM*|Hot*",ignore.case = TRUE,ds$EVTYPE)] <- "Heat"



tot_yearly<- ddply(ds,
                   .(EVTYPE_CAT,BGN_YEAR),
                   summarize,
                   tot_i=sum(INJURIES),
                   tot_f=sum(FATALITIES),
                   tot_pd=sum(PROPDMG),
                   tot_cd=sum(CROPDMG))

colnames(tot_yearly) <- c("EVENT_CATEGORY","YEAR","injuries","fatalities","prop_damage","crop_damage")


injuries_yearly <- tot_yearly[,c(1,2,3)]
colnames(injuries_yearly) <- c("EVENT_CATEGORY","YEAR","SUM")
injuries_yearly$IMPACT <- "INJURIES"

fatalities_yearly <- tot_yearly[,c(1,2,4)]
colnames(fatalities_yearly) <- c("EVENT_CATEGORY","YEAR","SUM")
fatalities_yearly$IMPACT <- "FATALITIES"

propdmg_yearly <- tot_yearly[,c(1,2,5)]
colnames(propdmg_yearly) <- c("EVENT_CATEGORY","YEAR","SUM")
propdmg_yearly$IMPACT <- "PROPDMG"

cropdmg_yearly <- tot_yearly[,c(1,2,6)]
colnames(cropdmg_yearly) <- c("EVENT_CATEGORY","YEAR","SUM")
cropdmg_yearly$IMPACT <- "CROPDMG"

consolidation <- rbind(injuries_yearly,fatalities_yearly,propdmg_yearly,cropdmg_yearly)


consolidation$EVENT_CATEGORY <- factor(consolidation$EVENT_CATEGORY)
consolidation$IMPACT <- factor(consolidation$IMPACT)

health_impact <- subset(consolidation, consolidation$IMPACT %in% c("INJURIES","FATALITIES"))


qplot(YEAR,SUM,data=health_impact,
             geom=c("line"),
             facets=EVENT_CATEGORY~IMPACT)



economic_impact <- subset(consolidation, consolidation$IMPACT %in% c("CROPDMG","PROPDMG"))

qplot(YEAR,SUM,data=economic_impact,
      +       geom=c("line"),
      +       facets=EVENT_CATEGORY~IMPACT)


qplot(YEAR,SUM,data=economic_impact,
      +       geom=c("line"),
      +       facets=EVENT_CATEGORY~IMPACT)




tot<- ddply(ds,
            .(EVTYPE_CAT),
            summarize,
            tot_i=sum(INJURIES),
            tot_f=sum(FATALITIES),
            tot_pd=sum(PROPDMG),
            tot_cd=sum(CROPDMG))
colnames(tot) <- c("EVENT_CATEGORY","injuries","fatalities","prop_damage","crop_damage")
tot_health_impact <- tot[,c(1,2,3)]
tot_health_impact <- tot_health_impact[order(-tot_health_impact$fatalities),]

tot_economic_impact <- tot[,c(1,4,5)]
tot_economic_impact$tot_damage <- tot_economic_impact$prop_damage + tot_economic_impact$crop_damage
tot_economic_impact$tot_damage <- tot_economic_impact[order(-tot_economic_impact$tot_damage),]






