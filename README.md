---
title: "A Look at SAT/ACT Test Scores as They Relate to Demographics and Outcome Measures at Public Universities in the Various Regions of the US"
author: "D'Ette Wagar"
date: "2024-03-27"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

### This is an analysis of the relationships between the summary SAT and ACT test score data reported by universities and the regions, demographic information, and awards conferred by those universities. Data is sourced from: https://nces.ed.gov. It specifically targets the 2014-2015 adjusted cohort.
<br>
```{r main}
install.packages("dplyr")
library(dplyr)
install.packages("tidyverse")
library(tidyverse)
install.packages("readr")
library(readr)
install.packages("tidyselect")
library(tidyselect)
install.packages("reshape2")
library(reshape2)
install.packages("ggplot2")
library(ggplot2)
install.packages("readxl")
library(readxl)
install.packages("RColorBrewer")
library(RColorBrewer)
install.packages("corrplot")
library(corrplot)

# Load 2014 admissions data
admissions_df <- read.csv("adm2014_rv_data_stata.csv")

# Truncate admissions data to omit irrelevant info, including corresponding
# X columns that indicate how data was obtained
admissions_trunc_df <- admissions_df %>%
  select(UNITID, ADMCON7, starts_with("APPL"), starts_with("XAPPL"),
           starts_with("ADMS"), starts_with("XADMS"),  starts_with("ENRL"),
           starts_with("XENRL"), starts_with("SAT"), starts_with("XSAT"),
           starts_with("ACT"), starts_with("XACT"))

# From the truncated admissions data frame, create two admissions data frames,
# one with SAT data and one with ACT data. Each data frame is filtered separately
# so that all strictly reported SAT data can be examined independently of all
# strictly reported ACT data.

# Both data sets are filtered such that data in a column is identified as
# reported with a value of "R" in that column's corresponding X column, per the
# data dictionary
admissions_SAT_rptd_df <- admissions_trunc_df %>%
  select (UNITID, ADMCON7, starts_with("APPL"), starts_with("XAPPL"),
            starts_with("ADMS"), starts_with("XADMS"), starts_with("ENRL"),
            starts_with("XENRL"), starts_with("SAT"), starts_with("XSAT")) %>%
  filter(if_all(starts_with("X"), ~ . == "R"))

admissions_ACT_rptd_df <- admissions_trunc_df %>%
  select (UNITID, ADMCON7, starts_with("APPL"), starts_with("XAPPL"),
            starts_with("ADMS"), starts_with("XADMS"), starts_with("ENRL"),
            starts_with("XENRL"), starts_with("ACT"), starts_with("XACT")) %>%
  filter(if_all(starts_with("X"), ~ . == "R"))

# Load institution data
institution_df <- read.csv("hd2022_data_stata.csv")

# Filter institution data to include unit ID, name, region,
# control (public, private), historically black indicator, tribal indicator,
# locale code, active indicator, congressional district code.

# Data is limited to only public institutions (CONTROL == 1) that are not
# historically black (HBCU != 1) and not tribal (TRIBAL != 1).
institution_trunc_df <- institution_df %>%
  select(UNITID, INSTNM, OBEREG, CONTROL, HBCU, TRIBAL, LOCALE,
           CYACTIVE, CNGDSTCD) %>%
  filter(CONTROL == 1, HBCU != 1, TRIBAL != 1)


# Load enrollment data (multiple entries per unit ID)
enrollment_df <- read.csv("effy2022_data_stata.csv")

# Filter enrollment data to:
# 1) include data that was strictly reported,
#   as represented in each column that is named beginning with an "X" that
#   has a value of "R" (per the data dictionary),
# 2) include all enrolled students (EFFYALEV = 1 & EFFYLEV = 1), yielding
#   1 row per unit ID
enrollment_rptd_df <- filter(enrollment_df, if_all(starts_with("X"), ~ . == "R" )
                             & EFFYALEV == 1 & EFFYLEV == 1)

# Truncate enrollment data to omit irrelevant info
enrollment_rptd_trunc_df <- enrollment_rptd_df %>%
  select(UNITID, starts_with("EFY"))


# Join SAT admissions data and ACT admissions data to institutional data
inst_SAT_df <- merge(admissions_SAT_rptd_df, institution_trunc_df, by = "UNITID")
inst_ACT_df <- merge(admissions_ACT_rptd_df, institution_trunc_df, by = "UNITID")

# Join SAT admissions/instutional data and ACT admissions/instutional data to
# enrollment data
inst_enr_SAT_df <- merge(inst_SAT_df, enrollment_rptd_trunc_df, by = "UNITID")
inst_enr_ACT_df <- merge(inst_ACT_df, enrollment_rptd_trunc_df, by = "UNITID")

## Find mean SAT scores by region
mean_SAT_df <- inst_enr_SAT_df %>%
  group_by(OBEREG) %>%
  summarize(SAT_V_25 = mean(SATVR25), SAT_V_75= mean(SATVR75), SAT_M_25 =
             mean(SATMT25), SAT_M_75 = mean(SATMT75), SAT_W_25 = 
             mean(SATWR25), SAT_W_75 = mean(SATWR75))

## Find mean ACT scores by region
mean_ACT_df <- inst_enr_ACT_df %>%
  group_by(OBEREG) %>%
  summarize(ACT_C_25 = mean(ACTCM25), ACT_C_75 = mean(ACTCM75),
            ACT_V_25 = mean(ACTEN25), ACT_V_75 = mean(ACTEN75),
            ACT_M_25 = mean(ACTMT25), ACT_M_75 = mean(ACTMT75),
            ACT_W_25 = mean(ACTWR25), ACT_W_75 = mean(ACTWR75))

# Reformat SAT data and display in bar graph
mean_SAT_df.long <- melt(mean_SAT_df,id.vars="OBEREG")
```
<br>

## SAT and ACT Scores by Region
<br>
```{r SAT/region plot, echo=FALSE}
library(ggplot2)
ggplot(mean_SAT_df.long,aes(x=variable,y=value,fill=factor(OBEREG)))+
  geom_bar(stat="identity",position="dodge")+
  labs(title="Comparison of SAT Scores by Region\nfor Fall 2014 Admissions\n")+
  scale_fill_manual(name="Bureau of Economic Analysis\n(BEA) Regions",
                      breaks=c(1, 2, 3, 4, 5, 6, 7, 8),
                      labels=c("New England", 
                               "Mid East", 
                               "Great Lakes",
                               "Plains",
                               "Southeast", 
                               "Southwest",
                               "Rocky Mountains", 
                               "Far West"),
                      values=c("#B2182B","#D6604D","#F4A582","#FDDBC7",
                              "#D1E5F0","#92C5DE","#4393C3", "#2166AC"))+
  xlab("Test and Percentile Level")+ylab("Average Score")+
  theme(axis.text.x = element_text(angle = 45))
```

```{r ACT reformat}
# Reformat ACT data and display in bar graph
library(reshape2)
mean_ACT_df.long <- melt(mean_ACT_df,id.vars="OBEREG")
```

```{r ACT/region plot, echo=FALSE}
library(ggplot2)
ggplot(mean_ACT_df.long,aes(x=variable,y=value,fill=factor(OBEREG)))+
  labs(title="Comparison of ACT Scores by Region\nfor Fall 2014 Admissions\n")+
  geom_bar(stat="identity",position="dodge")+
  scale_fill_manual(name="Bureau of Economic Analysis\n(BEA) Regions",
                      breaks=c(1, 2, 3, 4, 5, 6, 7, 8),
                      labels=c("New England", 
                               "Mid East", 
                               "Great Lakes",
                               "Plains",
                               "Southeast", 
                               "Southwest",
                               "Rocky Mountains", 
                               "Far West"),
                      values=c("#B2182B","#D6604D","#F4A582","#FDDBC7",
                              "#D1E5F0","#92C5DE","#4393C3", "#2166AC"))+
  xlab("Test and Percentile Level")+ylab("Average Score")+
  theme(axis.text.x = element_text(angle = 45))
```
<br>
<br>
<br>
<br>
```{r region graphic, out.width="50%", echo=FALSE, eval=TRUE}
  knitr::include_graphics("IMG_4535.jpeg")
```

##### States in Each Region

<br>
<br>

## Drill-Down Analysis of SAT and ACT Score Data
<br>

### We can see from the bar graphs above that regional performance is fairly consistent on both the SAT and ACT, both from test to test (verbal, math, etc.) and at the different percentiles of these tests.

### Because we see this consistency, future graphs will examine only the 75th percentile scores on the 3 SAT tests and the 75th percentile score on the ACT composite tests.


```{r High/low analysis}

# Let's take a look at how our high and low scoring regions break down by gender and race. For the SAT, we will focus on the Rocky Mountains and Far West regions on the high end and the Plains region on the low end. For the ACT, we will examine the Rocky Mountains and New England regions.

# Create rows of percentages

# Percent men enrolled
enrollment_rptd_trunc_df <- enrollment_rptd_trunc_df %>% mutate(PCT_MEN = as.integer(EFYTOTLM/EFYTOTLT * 100))

# Percent women enrolled
enrollment_rptd_trunc_df <- enrollment_rptd_trunc_df %>% mutate(PCT_WMN = as.integer(EFYTOTLW/EFYTOTLT * 100))

# Percent Asian
enrollment_rptd_trunc_df <- enrollment_rptd_trunc_df %>% mutate(PCT_ASI = as.integer(EFYASIAT/EFYTOTLT * 100))

# Percent Black
enrollment_rptd_trunc_df <- enrollment_rptd_trunc_df %>% mutate(PCT_BLA = as.integer(EFYBKAAT/EFYTOTLT * 100))

# Percent Hispanic or Latinx
enrollment_rptd_trunc_df <- enrollment_rptd_trunc_df %>% mutate(PCT_HIS = as.integer(EFYHISPT/EFYTOTLT * 100))

# Percent N. Hawaiian or O. Pacific Islander
enrollment_rptd_trunc_df <- enrollment_rptd_trunc_df %>% mutate(PCT_HAW = as.integer(EFYNHPIT/EFYTOTLT * 100))

# Percent White
enrollment_rptd_trunc_df <- enrollment_rptd_trunc_df %>% mutate(PCT_WHI = as.integer(EFYWHITT/EFYTOTLT * 100))

# Percent 2 or more Races
enrollment_rptd_trunc_df <- enrollment_rptd_trunc_df %>% mutate(PCT_2OR = as.integer(EFY2MORT/EFYTOTLT * 100))

# Percent Race Unknown
enrollment_rptd_trunc_df <- enrollment_rptd_trunc_df %>% mutate(PCT_UNK = as.integer(EFYUNKNT/EFYTOTLT * 100))

# **Gender percentages may not add up to 100, as some genders may bave been reported as "unknown" or "other".

# Join SAT admissions/instutional data and ACT admissions/instutional data to
# UPDATED enrollment data
upd_enr_SAT_df <- merge(inst_SAT_df, enrollment_rptd_trunc_df, by = "UNITID")
upd_enr_ACT_df <- merge(inst_ACT_df, enrollment_rptd_trunc_df, by = "UNITID")

## For SAT data, find the enrollment percentages by gender and race, by region and filter to view only the Rocky Mountains, Far West, and Plains regions
mean_gen_race_SAT_df <- upd_enr_SAT_df %>%
  group_by(OBEREG) %>%
  summarize(MEN = mean(PCT_MEN), WMN = mean(PCT_WMN), ASI = mean(PCT_ASI),
            BLA = mean(PCT_BLA), HIS = mean(PCT_HIS), HAW = mean(PCT_HAW),
            WHI = mean(PCT_WHI), TWO = mean(PCT_2OR), UNK = mean(PCT_UNK)) %>%
  filter(OBEREG %in% c(4,7,8))

## For ACT data, find the enrollment percentages by gender and race, by region and filter to view only the New England and Rocky Mountains regions
mean_gen_race_ACT_df <- upd_enr_ACT_df %>%
  group_by(OBEREG) %>%
  summarize(MEN = mean(PCT_MEN), WMN = mean(PCT_WMN), ASI = mean(PCT_ASI),
            BLA = mean(PCT_BLA), HIS = mean(PCT_HIS), HAW = mean(PCT_HAW),
            WHI = mean(PCT_WHI), TWO = mean(PCT_2OR), UNK = mean(PCT_UNK)) %>%
  filter(OBEREG %in% c(1,7))
  
# Reformat data and display in bar graph
mean_gen_race_SAT_df.long <- melt(mean_gen_race_SAT_df,id.vars="OBEREG")

mean_gen_race_ACT_df.long <- melt(mean_gen_race_ACT_df,id.vars="OBEREG")
```


```{r gender/race/SAT/region plot, echo=FALSE}
library(ggplot2)

#Display previous bar graph of SAT values by region again for ease of comparison
ggplot(mean_SAT_df.long,aes(x=variable,y=value,fill=factor(OBEREG)))+
  geom_bar(stat="identity",position="dodge")+
  labs(title="Comparison of SAT Scores by Region\nfor Fall 2014 Admissions\n")+
  scale_fill_manual(name="Bureau of Economic Analysis\n(BEA) Regions",
                      breaks=c(1, 2, 3, 4, 5, 6, 7, 8),
                      labels=c("New England", 
                               "Mid East", 
                               "Great Lakes",
                               "Plains",
                               "Southeast", 
                               "Southwest",
                               "Rocky Mountains", 
                               "Far West"),
                      values=c("#B2182B","#D6604D","#F4A582","#FDDBC7",
                              "#D1E5F0","#92C5DE","#4393C3", "#2166AC"))+
  xlab("Test and Percentile Level")+ylab("Average Score")+
  theme(axis.text.x = element_text(angle = 45))

# Display graph of demographic info corresponding to high/low SAT regions
ggplot(mean_gen_race_SAT_df.long,aes(x=variable,y=value,fill=factor(OBEREG)))+
  geom_bar(stat="identity",position="dodge")+
  labs(title="Comparison of the Demographic Profiles of the Regions Averaging the Highest\nSAT Scores in the 75th Percentile Ranges (Rocky Mountains and Far West)\nwith the Region Averaging the Lowest (Plains) in 2014\n")+
  scale_fill_manual(name="Bureau of Economic Analysis\n(BEA) Regions",
                      breaks=c(4, 7, 8),
                      labels=c("Plains",
                               "Rocky Mountains", 
                               "Far West"),
                    values=c("#FDDBC7", "#4393C3", "#2166AC"))+
  xlab("Demographic")+ylab("Percent of Total Enrolllment")+
  theme(axis.text.x = element_text(angle = 45))
```
<br>
<br>
<br>
```{r gender/race/ACT/region plot, echo=FALSE}  
# Display previous bar graph of ACT values by region again for ease of comparison
ggplot(mean_ACT_df.long,aes(x=variable,y=value,fill=factor(OBEREG)))+
  labs(title="Comparison of ACT Scores by Region\nfor Fall 2014 Admissions\n")+
  geom_bar(stat="identity",position="dodge")+
  scale_fill_manual(name="Bureau of Economic Analysis\n(BEA) Regions",
                      breaks=c(1, 2, 3, 4, 5, 6, 7, 8),
                      labels=c("New England", 
                               "Mid East", 
                               "Great Lakes",
                               "Plains",
                               "Southeast", 
                               "Southwest",
                               "Rocky Mountains", 
                               "Far West"),
                      values=c("#B2182B","#D6604D","#F4A582","#FDDBC7",
                              "#D1E5F0","#92C5DE","#4393C3", "#2166AC"))+
  xlab("Test and Percentile Level")+ylab("Average Score")+
  theme(axis.text.x = element_text(angle = 45))

# Display graph of demographic info corresponding to high/low ACT regions
ggplot(mean_gen_race_ACT_df.long,aes(x=variable,y=value,fill=factor(OBEREG)))+
  geom_bar(stat="identity",position="dodge")+
  labs(title="Comparison of the Demographic Profiles of the Region Averaging the Highest\n ACT Composite Score in the 75th Percentile Range (Rocky Mountains and)\nwith the Region Averaging the Lowest (New England) in 2014\n")+
  scale_fill_manual(name="Bureau of Economic Analysis\n(BEA) Regions",
                      breaks=c(1, 7),
                      labels=c("New England",
                               "Rocky Mountains"),
                    values=c("#B2182B", "#4393C3"))+
  xlab("Demographic")+ylab("Percent of Total Enrolllment")+
  theme(axis.text.x = element_text(angle = 45))
```
<br>
<br>

### Interestingly, lower math scores seem to be related to a greater percentage of women enrolled. Let's examine that more closely by looking at the statistical correlation coefficients between percent women enrolled and scores.
```{r SAT gender statistics}
# Create SAT data set containing just the values of unit ID, percent women enrolled, and SAT scores. Then create the correlation matrix.
gender_SAT_df <- inst_enr_SAT_df %>%
  mutate(PCT_WMN = as.integer(EFYTOTLW/EFYTOTLT * 100)) %>%
  select(UNITID, PCT_WMN, SATVR25, SATVR75, SATMT25,
         SATMT75, SATWR25, SATWR75)

colnames(gender_SAT_df) <- c('UNITID', 'PCT_WMN', 'SAT_V_25', 'SAT_V_75', 
                             'SAT_M_25', 'SAT_M_75', 'SAT_W_25', 'SAT_W_75')
```

```{r SAT correlation matrix, echo=FALSE}
round(cor(gender_SAT_df), digits = 2)
corrplot(cor(gender_SAT_df),
         method = "number",
         type = "upper")
```

### The 75th percentile SAT math scores and the the percent women variables have a correlation coefficient of -.29, indicating that they *might* be correlated. To investigate further, we conduct a null test of the hypothesis, supposing the opposite: that there is **no** connection between the two. If the resulting p value is less than .05, meaning that there is less than a 5% chance that the 2 variables are correlated, then you can reject the hypothesis that there is no connection between the two. 

```{r SAT 75th percentile math null hypothesis test}
# Null test of the hypothesis for the percent women variable and the SAT math
# 75th percentile score variable
cor.test(gender_SAT_df$PCT_WMN, gender_SAT_df$SAT_M_75)
```

### The p value is low enough for us to reject the null hypothesis that there is no relationship between the score and the percent of wome who took the test. Therefore, there **is** a relationship between the score and the percent of women who took the test.

### Let us see if that relationship holds for the 25th percentile test as well.

```{r SAT 25th percentile math null hypothesis test}
# Null test of the hypothesis for the percent women variable and the SAT math
# 25th percentile score variable
cor.test(gender_SAT_df$PCT_WMN, gender_SAT_df$SAT_M_25)
```

### Indeed, the p value is in the significant range for this test as well, so we come to the same conclusion for this test and percentile marker.

### To further verify these results, we examine the p-values of the verbal SAT tests, where we do not expect to see the same results:

```{r SAT verbal null hypothesis tests}
# Null test of the hypothesis for the percent women variable and the SAT verbal
# 75th percentile score variable
cor.test(gender_SAT_df$PCT_WMN, gender_SAT_df$SAT_V_75)

# Null test of the hypothesis for the percent women variable and the SAT verbal
# 25th percentile score variable
cor.test(gender_SAT_df$PCT_WMN, gender_SAT_df$SAT_V_25)
```

### Not surprisingly, we cannot reject the hypothesis that there is no relationship between the percent of women who took the verbal test, at either score level, since the p value of each test is greater than .05.
<br>
<br>

### Moving on to the ACT math test, we see that the 75th percentile score is showing a correlation coefficient of -.51 with a p value of less than .01, which indicates that these variables are correlated. Although the correlation coefficient associated with percent women and the 25th percentile math ACT score is -.27, which is similar to the correlation coefficient we saw with the 25th percentile SAT score, the associated p value is .127, which is not low enough to reject the null hypothesis. Therefore, we cannot conclude that there is a correlation between the percent women variable and the ACT math 25th percentile score. 
```{r ACT gender statistics}
# Create data set containing just the values of unit ID, percent women enrolled, and SAT scores. Then create the correlation matrix.
gender_ACT_df <- inst_enr_ACT_df %>%
  mutate(PCT_WMN = as.integer(EFYTOTLW/EFYTOTLT * 100)) %>%
  select(UNITID, PCT_WMN, ACTCM25, ACTCM75, ACTEN25,
         ACTEN75, ACTMT25, ACTMT75, ACTWR25, ACTWR75)

colnames(gender_ACT_df) <- c('UNITID', 'PCT_WMN', 'ACT_C_25', 'ACT_C_75',
                             'ACT_V_25', 'ACT_V_75', 'ACT_M_25', 'ACT_M_75',
                             'ACT_W_25', 'ACT_W_75')
```

```{r ACT correlation matrix, echo=FALSE}
round(cor(gender_ACT_df), digits = 2)
corrplot(cor(gender_ACT_df),
         method = "number",
         type = "upper")
```

```{r ACT math null hypothesis tests}
# Null test of the hypothesis for the percent women variable and the ACT math
# 75th percentile score variable
cor.test(gender_ACT_df$PCT_WMN, gender_ACT_df$ACT_M_75)

# Null test of the hypothesis for the percent women variable and the ACT math
# 25th percentile score variable
cor.test(gender_ACT_df$PCT_WMN, gender_ACT_df$ACT_M_25)
```
<br>
<br>

## SAT and ACT Scores, in Comparison to Percent of Awards (Certificates or Degrees) Conferred at 4, 6, and 8 Years, by Region
<br>

### Finally, let's examine overall region's average SAT and ACT scores in relation to their average percent awards conferred at 4, 6, and 8 years. Awards include certificates or degrees.
```{r outcome measures}

# Load outcome measure data
outcome_df <- read.csv("om2022_data_stata.csv")

# Truncate outcome data to include only percent of adjusted cohort to receive  
# awards at 4, 6, and 8 years, filtering to include only data reported by each   
# institution and data for all entering students in the 2014-2015 cohort
#(OMCHRT=50). (Setting OMCHRT=50 limits output to one row per UNITID.)
outcome_trunc_df <- outcome_df %>%
  select(UNITID, OMCHRT, OMAWDP4, XOMAWDP4, OMAWDP6, XOMAWDP6, OMAWDP8,
         XOMAWDP8) %>%
   filter(if_all(starts_with("X"), ~ . == "R"), OMCHRT==50)

# Join SAT institutional/enrollment data to outcome data
outcome_SAT_df <- merge(inst_enr_SAT_df, outcome_trunc_df, by = "UNITID")

# Find mean SAT scores and mean percentage of awards conferred per region
mean_outcome_df <- outcome_SAT_df %>%
  group_by(OBEREG) %>%
  summarize(SAT_V_25TH = mean(SATVR25), SAT_V_75TH= mean(SATVR75), 
            SAT_M_25TH = mean(SATMT25), SAT_M_75TH = mean(SATMT75), 
            SAT_W_25TH = mean(SATWR25), SAT_W_75TH = mean(SATWR75), 
            PCT_AWD_4Y = mean(OMAWDP4), PCT_AWD_6Y = mean(OMAWDP6),
            PCT_AWD_8Y = mean(OMAWDP8))

# Separate out just the SAT scores
mean_outcome_SAT_df <- mean_outcome_df %>%
  select(OBEREG, SAT_V_25TH, SAT_V_75TH, SAT_M_25TH, SAT_M_75TH, SAT_W_25TH,
         SAT_W_75TH)

# Reformat mean SAT data and display in bar graph
mean_outcome_SAT_df.long <- melt(mean_outcome_SAT_df,id.vars="OBEREG")

# Separate out just the mean percent awards
mean_outcome_awd_df <- mean_outcome_df %>%
  select(OBEREG, PCT_AWD_4Y, PCT_AWD_6Y, PCT_AWD_8Y)

# Reformat mean percent award data and display in bar graph
mean_outcome_awd_df.long <- melt(mean_outcome_awd_df,id.vars="OBEREG")
```
<br>

### SAT Scores, along with Awards Conferred, by Region
<br>
```{r SAT/awards/region plot, echo=FALSE}
library(ggplot2)
ggplot(mean_outcome_SAT_df.long,aes(x=variable,y=value,fill=factor(OBEREG)))+
  geom_bar(stat="identity",position="dodge")+
  labs(title="Comparison of SAT Scores by Region\n
       for the 2014-2015 Cohort\n")+
  scale_fill_manual(name="Bureau of Economic Analysis\n(BEA) Regions",
                      breaks=c(1, 2, 3, 4, 5, 6, 7, 8),
                      labels=c("New England", 
                               "Mid East", 
                               "Great Lakes",
                               "Plains",
                               "Southeast", 
                               "Southwest",
                               "Rocky Mountains", 
                               "Far West"),
                      values=c("#B2182B","#D6604D","#F4A582","#FDDBC7",
                              "#D1E5F0","#92C5DE","#4393C3", "#2166AC"))+
  xlab("Test and Percentile Level")+
  ylab("Average Score")+
  theme(axis.text.x = element_text(angle = 45))

ggplot(mean_outcome_awd_df.long,aes(x=variable,y=value,fill=factor(OBEREG)))+
  geom_bar(stat="identity",position="dodge")+
  labs(title="Comparison of Awards Conferred\n
       for the 2014-2015 Cohort\n")+
  scale_fill_manual(name="Bureau of Economic Analysis\n(BEA) Regions",
                      breaks=c(1, 2, 3, 4, 5, 6, 7, 8),
                      labels=c("New England", 
                               "Mid East", 
                               "Great Lakes",
                               "Plains",
                               "Southeast", 
                               "Southwest",
                               "Rocky Mountains", 
                               "Far West"),
                      values=c("#B2182B","#D6604D","#F4A582","#FDDBC7",
                              "#D1E5F0","#92C5DE","#4393C3", "#2166AC"))+
  xlab("Years After Enrollment")+
  ylab("Percentage of Enrolled Students Receiving Awards")+
  theme(axis.text.x = element_text(angle = 45))
```

### There appear to be similarities between the test data and the outcome measure data; the regions with higher test scores tend to have larger percentages of awards conferred and vice versa. Let's create a correlation matrix to explore those relationships further. 
```{r SAT outcome correlation, echo=FALSE}
# Create a correlation matrix to see if there is a relationship between scores 
# degrees conferred
num_outcome_SAT_df <- outcome_SAT_df %>%
  select(UNITID, SATVR25, SATVR75, SATMT25, SATMT75, SATWR25, SATWR75, 
         OMAWDP4, OMAWDP6, OMAWDP8)

colnames(num_outcome_SAT_df) <- c('UNITID', 'SAT_V_25', 'SAT_V_75', 'SAT_M_25', 'SAT_M_75', 'SAT_W_25', 'SAT_W_75', 'PCT_AWD_4Y', 'PCT_AWD_6Y', 'PCT_AWD_8Y')

round(cor(num_outcome_SAT_df), digits = 2)
corrplot(cor(num_outcome_SAT_df),
         method = "number",
         type = "upper")
```

### The correlation matrix indicates that there could be correlations among each of the test variables and each of the award variables. The following null hypothesis tests confirm that indeed there is a correlation in every case.
```{r SAT awards null hypothesis tests, echo=FALSE}

#Null hypothesis tests for 4-year awards
cor.test(num_outcome_SAT_df$PCT_AWD_4Y, num_outcome_SAT_df$SAT_V_25)

cor.test(num_outcome_SAT_df$PCT_AWD_4Y, num_outcome_SAT_df$SAT_V_75)

cor.test(num_outcome_SAT_df$PCT_AWD_4Y, num_outcome_SAT_df$SAT_M_25)

cor.test(num_outcome_SAT_df$PCT_AWD_4Y, num_outcome_SAT_df$SAT_M_75)

cor.test(num_outcome_SAT_df$PCT_AWD_4Y, num_outcome_SAT_df$SAT_W_25)

cor.test(num_outcome_SAT_df$PCT_AWD_4Y, num_outcome_SAT_df$SAT_W_75)

#Null hypothesis tests for 6-year awards
cor.test(num_outcome_SAT_df$PCT_AWD_6Y, num_outcome_SAT_df$SAT_V_25)

cor.test(num_outcome_SAT_df$PCT_AWD_6Y, num_outcome_SAT_df$SAT_V_75)

cor.test(num_outcome_SAT_df$PCT_AWD_6Y, num_outcome_SAT_df$SAT_M_25)

cor.test(num_outcome_SAT_df$PCT_AWD_6Y, num_outcome_SAT_df$SAT_M_75)

cor.test(num_outcome_SAT_df$PCT_AWD_6Y, num_outcome_SAT_df$SAT_W_25)

cor.test(num_outcome_SAT_df$PCT_AWD_6Y, num_outcome_SAT_df$SAT_W_75)

#Null hypothesis tests for 8-year awards
cor.test(num_outcome_SAT_df$PCT_AWD_8Y, num_outcome_SAT_df$SAT_V_25)

cor.test(num_outcome_SAT_df$PCT_AWD_8Y, num_outcome_SAT_df$SAT_V_75)

cor.test(num_outcome_SAT_df$PCT_AWD_8Y, num_outcome_SAT_df$SAT_M_25)

cor.test(num_outcome_SAT_df$PCT_AWD_8Y, num_outcome_SAT_df$SAT_M_75)

cor.test(num_outcome_SAT_df$PCT_AWD_8Y, num_outcome_SAT_df$SAT_W_25)

cor.test(num_outcome_SAT_df$PCT_AWD_8Y, num_outcome_SAT_df$SAT_W_75)
```
<br>
```{r outcome measures 2}
# Join ACT institutional/enrollment data to outcome data
outcome_ACT_df <- merge(inst_enr_ACT_df, outcome_trunc_df, by = "UNITID")

# Find mean ACT scores and mean percentage of awards conferred per region
mean_outcome2_df <- outcome_ACT_df %>%
  group_by(OBEREG) %>%
summarize(ACT_C_25 = mean(ACTCM25), ACT_C_75 = mean(ACTCM75),
          ACT_V_25 = mean(ACTEN25), ACT_V_75 = mean(ACTEN75),
          ACT_M_25 = mean(ACTMT25), ACT_M_75 = mean(ACTMT75),
          ACT_W_25 = mean(ACTWR25), ACT_W_75 = mean(ACTWR75),
          PCT_AWD_4Y = mean(OMAWDP4), PCT_AWD_6Y = mean(OMAWDP6),
          PCT_AWD_8Y = mean(OMAWDP8))

# Separate out just the ACT scores
mean_outcome_ACT_df <- mean_outcome2_df %>%
select(OBEREG, ACT_C_25, ACT_C_75, ACT_V_25, ACT_V_75, ACT_M_25,
       ACT_M_75, ACT_W_25, ACT_W_75)

# Reformat mean ACT data and display in bar graph
mean_outcome_ACT_df.long <- melt(mean_outcome_ACT_df,id.vars="OBEREG")

# Separate out just the mean percent awards
mean_outcome_awd2_df <- mean_outcome2_df %>%
  select(OBEREG, PCT_AWD_4Y, PCT_AWD_6Y, PCT_AWD_8Y)

# Reformat mean percent award data and display in bar graph
mean_outcome_awd2_df.long <- melt(mean_outcome_awd2_df,id.vars="OBEREG")
```
<br>

### ACT Scores, along with Awards Conferred, by Region
```{r ACT/awards/region plot, echo=FALSE}
library(ggplot2)
ggplot(mean_outcome_ACT_df.long,aes(x=variable,y=value,fill=factor(OBEREG)))+
  geom_bar(stat="identity",position="dodge")+
  labs(title="Comparison of ACT Scores by Region\n
       for the 2014-2015 Cohort\n")+
  scale_fill_manual(name="Bureau of Economic Analysis\n(BEA) Regions",
                      breaks=c(1, 2, 3, 4, 5, 6, 7, 8),
                      labels=c("New England", 
                               "Mid East", 
                               "Great Lakes",
                               "Plains",
                               "Southeast", 
                               "Southwest",
                               "Rocky Mountains", 
                               "Far West"),
                      values=c("#B2182B","#D6604D","#F4A582","#FDDBC7",
                              "#D1E5F0","#92C5DE","#4393C3", "#2166AC"))+
  xlab("Test and Percentile Level")+
  ylab("Average Score")+
  theme(axis.text.x = element_text(angle = 45))

ggplot(mean_outcome_awd2_df.long,aes(x=variable,y=value,fill=factor(OBEREG)))+
  geom_bar(stat="identity",position="dodge")+
  labs(title="Comparison of Awards Conferred\n
       for the 2014-2015 Cohort\n")+
  scale_fill_manual(name="Bureau of Economic Analysis\n(BEA) Regions",
                      breaks=c(1, 2, 3, 4, 5, 6, 7, 8),
                      labels=c("New England", 
                               "Mid East", 
                               "Great Lakes",
                               "Plains",
                               "Southeast", 
                               "Southwest",
                               "Rocky Mountains", 
                               "Far West"),
                      values=c("#B2182B","#D6604D","#F4A582","#FDDBC7",
                              "#D1E5F0","#92C5DE","#4393C3", "#2166AC"))+
  xlab("Years After Enrollment")+
  ylab("Percentage of Enrolled Students Receiving Awards")+
  theme(axis.text.x = element_text(angle = 45))
```

### Once again, there appear to be similarities between the test data and the outcome measure data. We create a correlation matrix to take a closer look. 
```{r ACT outcome correlation, echo=FALSE}
# Create a correlation matrix to see if there is a relationship between scores 
# degrees conferred
num_outcome_ACT_df <- outcome_ACT_df %>%
  select(UNITID, ACTCM25, ACTCM75, ACTEN25,
         ACTEN75, ACTMT25, ACTMT75, ACTWR25, ACTWR75, 
         OMAWDP4, OMAWDP6, OMAWDP8)

colnames(num_outcome_ACT_df) <- c('UNITID', 'ACT_C_25', 'ACT_C_75', 'ACT_V_25',
                                  'ACT_V_75', 'ACT_M_25', 'ACT_M_75', 'ACT_W_25',
                                  'ACT_W_75', 'PCT_AWD_4Y', 'PCT_AWD_6Y', 
                                  'PCT_AWD_8Y')

round(cor(num_outcome_ACT_df), digits = 2)
corrplot(cor(num_outcome_ACT_df),
         method = "number",
         type = "upper")
```

### As with the SAT tests, the correlation matrix indicates that there could be correlations among each of the test variables and each of the award variables. Also as with the SAT tests, the following null hypothesis tests confirm that indeed there is a correlation in every case. Interestingly, though, the correlation coefficients for the different score percentiles within the ACT are not as consistent as they are within the SAT. Although outside the scope of this analysis, an exploration into the reasons for these differences might be worthwhile.
```{r ACT awards null hypothesis tests, echo=FALSE}
#Null hypothesis tests for 4-year awards
cor.test(num_outcome_ACT_df$PCT_AWD_4Y, num_outcome_ACT_df$ACT_C_25)

cor.test(num_outcome_ACT_df$PCT_AWD_4Y, num_outcome_ACT_df$ACT_C_75)

cor.test(num_outcome_ACT_df$PCT_AWD_4Y, num_outcome_ACT_df$ACT_V_25)

cor.test(num_outcome_ACT_df$PCT_AWD_4Y, num_outcome_ACT_df$ACT_V_75)

cor.test(num_outcome_ACT_df$PCT_AWD_4Y, num_outcome_ACT_df$ACT_M_25)

cor.test(num_outcome_ACT_df$PCT_AWD_4Y, num_outcome_ACT_df$ACT_M_75)

cor.test(num_outcome_ACT_df$PCT_AWD_4Y, num_outcome_ACT_df$ACT_W_25)

cor.test(num_outcome_ACT_df$PCT_AWD_4Y, num_outcome_ACT_df$ACT_W_75)


#Null hypothesis tests for 6-year awards
cor.test(num_outcome_ACT_df$PCT_AWD_6Y, num_outcome_ACT_df$ACT_C_25)

cor.test(num_outcome_ACT_df$PCT_AWD_6Y, num_outcome_ACT_df$ACT_C_75)

cor.test(num_outcome_ACT_df$PCT_AWD_6Y, num_outcome_ACT_df$ACT_V_25)

cor.test(num_outcome_ACT_df$PCT_AWD_6Y, num_outcome_ACT_df$ACT_V_75)

cor.test(num_outcome_ACT_df$PCT_AWD_6Y, num_outcome_ACT_df$ACT_M_25)

cor.test(num_outcome_ACT_df$PCT_AWD_6Y, num_outcome_ACT_df$ACT_M_75)

cor.test(num_outcome_ACT_df$PCT_AWD_6Y, num_outcome_ACT_df$ACT_W_25)

cor.test(num_outcome_ACT_df$PCT_AWD_6Y, num_outcome_ACT_df$ACT_W_75)


#Null hypothesis tests for 8-year awards
cor.test(num_outcome_ACT_df$PCT_AWD_8Y, num_outcome_ACT_df$ACT_C_25)

cor.test(num_outcome_ACT_df$PCT_AWD_8Y, num_outcome_ACT_df$ACT_C_75)

cor.test(num_outcome_ACT_df$PCT_AWD_8Y, num_outcome_ACT_df$ACT_V_25)

cor.test(num_outcome_ACT_df$PCT_AWD_8Y, num_outcome_ACT_df$ACT_V_75)

cor.test(num_outcome_ACT_df$PCT_AWD_8Y, num_outcome_ACT_df$ACT_M_25)

cor.test(num_outcome_ACT_df$PCT_AWD_8Y, num_outcome_ACT_df$ACT_M_75)

cor.test(num_outcome_ACT_df$PCT_AWD_8Y, num_outcome_ACT_df$ACT_W_25)

cor.test(num_outcome_ACT_df$PCT_AWD_8Y, num_outcome_ACT_df$ACT_W_75)
```

### Conclusion: There are statistically significant correlations between math SAT and ACT scores and gender. Specifically, universities with higher female populations report lower math scores. Additionally, higher scores correlate with a greater percentage of awards conferred by a unversity.
