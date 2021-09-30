

# SETUP -------------------------------------------------------------------

if (!require("librarian")) install.packages("librarian")
librarian::shelf(tidyverse, here)

# Notes on code, naming, and flow below: 

# stakeholders distinguished like: 
  # PH = public health; BEV = beverage industry; LEG = federal legislature; IRS = Internal Revenue Service; UNK = unknown

# phases distinguished like: 
  # EXP = Exploration; PREP = Preparation; IMP = Implementation; SUS = Sustainment. 

# all costs are inflated to 2021 dollars, using JANUARY inflation

# all costs are rounded to three significant digits to avoid over precision
  sigDigs = 3







# INFLATION ---------------------------------------------------------------

# CPI stats from here: https://data.bls.gov/PDQWeb/cu
# 2000 - 2021, all urban consumers, seasonally adjusted
# (since we are using a ratio, the seasonal vs. not seasonal adjustment does not make a big difference in the inflation factors)
# use numbers from January
# previous import: CPI_orig <- readxl::read_excel(here("data", "CPI_seasonally-adjusted_2000-2021.xlsx"), skip = 10)
# pasted as tribble from excel sheet downloaded from above link to allow the code to be fully self contained. 
  
  CPI <- tibble::tribble(~Year,    ~Jan,
                         2000L,   169.3,
                         2001L,   175.6,
                         2002L,   177.7,
                         2003L,   182.6,
                         2004L,   186.3,
                         2005L,   191.6,
                         2006L,   199.3,
                         2007L, 203.437,
                         2008L, 212.174,
                         2009L, 211.933,
                         2010L, 217.488,
                         2011L, 221.187,
                         2012L, 227.842,
                         2013L, 231.679,
                         2014L, 235.288,
                         2015L, 234.747,
                         2016L, 237.652,
                         2017L,  243.62,
                         2018L, 248.721,
                         2019L, 252.441,
                         2020L, 258.687,
                         2021L, 262.231 ) %>%
  pivot_wider(names_from = "Year", 
              values_from = "Jan") %>%
  janitor::clean_names() %>%
  as_vector()

# inflation factors are calculated from this named vector like so: 
  # CPI["x2021"]/CPI["x2009"]
# and costs are often 'unnamed' because we are using inflation factors from the named vector discussed above 




# PUBLIC HEALTH -----------------------------------------------------------

# CSPI 2020 annual financial report: https://cspinet.org/sites/default/files/CSPI_Financial_Audit-FY2020.pdf
# updated numbers below during final code review 9/29/21

# Coalition building
  specialProjectsCSPI_2020 <- 4545757
  
  # one third of the special projects expenditures used as an estimate for an SSB tax policy coalition
  # inflated to 2021 dollars
  PH_EXP_coalition <- (specialProjectsCSPI_2020*(1/3)*
                         (CPI["x2021"]/CPI["x2020"])) %>%
    signif(digits=sigDigs) %>%
    unname()


# Mass media campaign
  publicEducationCSPI_2020 <- 3257865
  
  # one third of the public education expenditures used as an estimate for a mass media campaign re: SSB tax
  PH_IMP_massMedia <- (publicEducationCSPI_2020*(1/3)*(CPI["x2021"]/CPI["x2020"])) %>%
    signif(digits=sigDigs) %>%
    unname()







# FEDERAL LEGISLATURE -----------------------------------------------------

# Decide on need for sugary drink tax policy
  # assumed 0
  LEG_EXP_needForTax = 0


# Enact legislation
  # Using the process set out by Wilson et al, 2012
  # Wilson N, Nghiem N, Foster R, Cobiac L, Blakely T. Estimating the cost of new public health legislation. Bulletin of the World Health Organization. 2012;90:532-539.
  # The approach essentially allocates how much of the total legislative budget is spent on actual policymaking activities and normalizes that per pieces of enacted legislation
  
  # CMF report: https://www.congressfoundation.org/storage/documents/CMF_Pubs/life-in-congress-the-member-perspective.pdf

  # not including 117th congress since it began Jan 3, 2021
  # number of acts source: https://www.govtrack.us/congress/bills/statistics
  # checked numbers 9/29/21
  acts <- tribble(~Congress, ~enactedLaws,
                  116, 344, 
                  115, 443, 
                  114, 329, 
                  113, 296,
                  112, 284, 
                  111, 385, 
                  110, 460, 
                  109, 483, 
                  108, 504, 
                  107, 383, 
                  106, 604)


  # legislative days source: https://history.house.gov/Institution/Session-Dates/All/
  # checked numbers 9/29/21
  legislativeDays <- tribble(~Congress, ~Session, ~LegislativeDays, 
                             116, 2, 163, 
                             116, 1, 193, 
                             115, 2, 174, 
                             115, 1, 192, 
                             114, 2, 131, 
                             114, 1, 157, 
                             113, 2, 135, 
                             113, 1, 160, 
                             112, 2, 153, 
                             112, 1, 175, 
                             111, 2, 127, 
                             111, 1, 159, 
                             110, 2, 119, 
                             110, 1, 164, 
                             109, 2, 101, 
                             109, 1, 140, 
                             108, 2, 110, 
                             108, 1, 133, 
                             107, 2, 123, 
                             107, 1, 142, 
                             106, 2, 135, 
                             106, 1, 137)



  # STEP 1: identify proportion of time legislators spend on legislative activities
  # use information on time spent on 'Legislative/Policy Work' from CMF report
  # the CMF report tells us that this is 35% while in DC, and 12% while in their district
  # but we need to get a proportion that is weighted by how much time they spend in each place (since it is not equal) 
  # checked 0.35 and 0.12 9/29/21
  dcLegislativeTime <- 0.35 
  districtLegislativeTime <- 0.12 
  
  
  # assume 364 days per year; assume legislators work 5 days per week (so minus two weekend days per week)
  # assume 20 additional days completely off (major holidays, sick, personal etc.)
  # embedded assumption is that they are not working weekends
  workingDays <- 364-(52*2)-20   
  workingWeeks <- workingDays/7
  
  # DC days will be assumed to equal legislative days
  # we will use the average legislative days per year (each congress has 2 sessions, since congresses span 2 years)
  dcAnnualDays <- round(mean(legislativeDays$LegislativeDays))
  dcAnnualWeeks <- dcAnnualDays/7
  
  # weeks spent in their district is therefore the possible working weeks minus weeks in DC
  districtAnnualWeeks <- workingWeeks-dcAnnualWeeks

  
  # would be great if we could use these weeks to allocate legislative time, but we also know that legislators work different hours in different places
  # so we should really be allocated HOURS, not WEEKS
  # CMF report estimated 70 while in DC, and 59 while in chamber 'out of session'
  # assume out of session == district, supported by CMF report
  # checked 70 and 59, 9/29/21
  dcHoursPerWeek <- 70  
  dcAnnualHours <- dcAnnualWeeks*dcHoursPerWeek
  # and now allocate 35% of those hours to legislative time per CMF report
  dcAnnualLegislativeHours <- dcAnnualHours*dcLegislativeTime
  
  districtHoursPerWeek <- 59 
  districtAnnualHours <- districtAnnualWeeks*districtHoursPerWeek
  # and now allocate 12% of those hours to legislative time per CMF report
  districtAnnualLegislativeHours <- districtAnnualHours*districtLegislativeTime
  
  totalAnnualLegislativeHours <- dcAnnualLegislativeHours + districtAnnualLegislativeHours
  totalAnnualHours <- dcAnnualHours + districtAnnualHours
  
  proportionLegislativeTime <- totalAnnualLegislativeHours/totalAnnualHours
  
  # assume this proportion of legislative time applies to current legislators
  
  
  # STEP 2: using this proportion, estimate how much of the total legislative budget is therefore spent on actual legislative actions
  # embedded in this step is the assumption that LEGISLATORS time spent on legislative activities is a reasonable proxy for
  # ALL PEOPLES time spent on legislative activities
  # and the assumption that these older data on time use still apply
  
  # an alternative would be to just take ALL legislative outlays and average over the enacted pieces of legislation. 
  # further work should try to refine this

  # use recent data on outlays, Table 4.1, Legislative Branch outlays
  # https://www.whitehouse.gov/omb/historical-tables/
  # updated and checked 9/29/21
  legislativeOutlays_2020 <- 5369*1000000
  legislativeOutlays_2021 <- legislativeOutlays_2020*(CPI["x2021"]/CPI["x2020"])
  
  outlaysSpentOnLegislativeActivities <- legislativeOutlays_2021*proportionLegislativeTime
  
  # (data on this above)
  actsPerLegislativeSession <- mean(acts$enactedLaws)
  actsPerYear <- actsPerLegislativeSession/2

  
  LEG_PREP_outlaysPerEnactedLegislation <- (outlaysSpentOnLegislativeActivities/actsPerYear) %>%
    signif(digits=sigDigs) %>%
    unname()
  







# INTERNAL REVENUE SERVICE ------------------------------------------------

  # data here from TTB annual report
  # https://www.ttb.gov/about-ttb/plans-and-reports-annual-reports
  # updated on 9/29/21
    
  # distribution of tax collections
  tobaccoProportion <- 0.56
  alcoholProportion <- 0.41
  estimatedSSBCollection <- mean(c(tobaccoProportion, alcoholProportion))
  

  collectTheRevenueTTB <- 52181000
  collectAlcTobaccoTax <- 0.91
  voluntaryComplianceProgram <- 0.06
  
  
  # Tax Collection
    
    # proportion collection and monitoring dollars goes to collection (collectAlcTobaccoTax)
    # and that's over tobacco/alc, so assume SSB would fall in about the average of those 2
    ssbTaxCollection <- (collectTheRevenueTTB*(collectAlcTobaccoTax)*(estimatedSSBCollection)*(CPI["x2021"]/CPI["x2020"])) %>%
      signif(digits=sigDigs) %>%
      unname()
    
    # doubled during initial implementation year
    IRS_IMP_taxCollection <- ssbTaxCollection*2

    # and that amount thereafter
    IRS_SUS_taxCollection <- ssbTaxCollection

  
  
  # Education
    
    # proportion of of collection and monitoring dollars spent on educational programs
    # again assume average proportion of tobacco/alc would be needed for SSBs
    IRS_SUS_education <- (collectTheRevenueTTB*(voluntaryComplianceProgram)*(estimatedSSBCollection)*(CPI["x2021"]/CPI["x2020"])) %>%
      signif(digits = sigDigs) %>%
      unname()
    
    
    
  # Revenue
    
    # Gortmaker, 2015: 10.1377/hlthaff.2015.0631
    # 12.5 billion
    # checked 9/29/21
    IRS_SUS_taxRevenue <- ((12.5*1000000000)*(CPI["x2021"]/CPI["x2015"])) %>%
      signif(digits = sigDigs) %>%
      unname()
    
    
  # Litigation
    IRS_IMP_ligitation <- as.double(NA)
    IRS_SUS_litigation <- as.double(NA)
    
    
  
  
  



## @knitr bev
# BEVERAGE INDUSTRY -------------------------------------------------------

# Advocacy
  BEV_EXP_advocacy = 0


# Lobbying
    # checked 9/29/21
  # using the incremental 2009 lobbying expenditures from the below article as base
  # https://cspinet.org/news/soda-industry-spent-67-million-opposing-state-city-soda-taxes-warning-labels-20160921
    # use the industry trade/two companies spending 'upwards of 14 million a year at the federal level'
    # and info on 'peak of 40 million in 2009'
  lobbying2009 <- 26000000
  BEV_PREP_lobbying <- lobbying2009*(CPI["x2021"]/CPI["x2009"]) %>%
    signif(digits=sigDigs) %>%
    unname()
  
  


# Compliance

  # assume both implementation and sustainment costs are equivalent to the federal government
  BEV_IMP_compliance <- IRS_IMP_taxCollection
  BEV_SUS_compliance <- IRS_SUS_taxCollection
  

# Payments
  # tax payments by the beverage industry should equal that of the IRS revenue
  BEV_SUS_taxPayments <- IRS_SUS_taxRevenue
  
# Reformulation
  BEV_SUS_reformulation <- 0


# Litigation
  BEV_IMP_ligitation <- as.double(NA)
  BEV_SUS_litigation <- as.double(NA)




# UNKNOWN -----------------------------------------------------------------

UNK_SUS_revenueAllocation <- as.double(NA)















  

  
## @knitr combination
# COMBINE -----------------------------------------------------------------

  total <- tribble(~stakeholder, ~phase, ~action, ~timing, ~data, 
                   
                   "ph", "exp", "Coalition", "yearly until legislation", PH_EXP_coalition, 
                   "ph", "imp", "Mass media", "one time", PH_IMP_massMedia, 
                   
                   
                   "bev", "exp", "Anti-regulation advocacy", "yearly", BEV_EXP_advocacy, 
                   "bev", "prep", "Lobbying","one time", BEV_PREP_lobbying, 
                   "bev", "imp", "Litigation", "one time", BEV_IMP_ligitation, 
                   "bev", "imp", "Compliance", "one time", BEV_IMP_compliance, 
                   "bev", "sus", "Litigation", "yearly", BEV_SUS_litigation,
                   "bev", "sus", "Reformulation", "yearly", BEV_SUS_reformulation,
                   "bev", "sus", "Compliance", "yearly", BEV_SUS_compliance, 
                   "bev", "sus", "Tax payments", "yearly", BEV_SUS_taxPayments, 
                   
                   
                   "leg", "exp", "Pursue policy", "one time", LEG_EXP_needForTax, 
                   "leg", "prep", "Enact legislation", "one time", LEG_PREP_outlaysPerEnactedLegislation,
                   

                   "irs", "imp", "Litigation", "one time", IRS_IMP_ligitation,
                   "irs", "imp", "Develop collection infrastructure", "one time", IRS_IMP_taxCollection, 
                   "irs", "sus", "Litigation", "yearly", IRS_SUS_litigation,
                   "irs", "sus", "Collection", "yearly", IRS_SUS_taxCollection, 
                   "irs", "sus", "Education", "yearly", IRS_SUS_education,
                   "irs", "sus", "Tax revenue", "yearly", IRS_SUS_taxRevenue, 
                   
                   
                   "unk", "sus", "Revenue allocation", "yearly", UNK_SUS_revenueAllocation)
  
  
  nicer <- total %>%
    mutate(Stakeholder = factor(stakeholder, 
                                levels = c("ph", "bev", "leg", "irs", "unk"),
                                labels = c("ph" = "Public Health", 
                                           "bev" = "Beverage Industry", 
                                           "leg" = "Federal Legislature",
                                           "irs" = "Internal Revenue Service", 
                                           "unk" = "Unknown")), 
           phasef = factor(phase, 
                           levels = c("exp", "prep", "imp", "sus"), 
                           labels = c("exp" = "Exploration", 
                                      "prep" = "Preparation", 
                                      "imp" = "Implementation", 
                                      "sus" = "Sustainment")), 
           dollars = scales::dollar(data))
  
  
  nicer %>% 
    select(stakeholder, phasef, dollars, action, timing) %>% 
    reactable::reactable(filterable = TRUE, 
                         defaultPageSize = 21, 
                         searchable = TRUE, 
                         showSortable = TRUE)
  
  
library(officer)
library(flextable)
  
createTable <- nicer %>%
  group_by(Stakeholder, phase) %>%
  mutate(extraID = row_number()) %>%
  ungroup() %>%
  mutate(dollars = as.character(dollars),
         dollars = if_else(is.na(dollars), "uncertain", dollars), 
         label = paste0(dollars, "\n  (", action, ")")) %>%
  pivot_wider(id_cols = c("Stakeholder", "extraID"), 
              names_from = "phasef", 
              values_from = "label") %>%
  select(-extraID) %>%
  as_grouped_data(groups = "Stakeholder") %>%
  flextable(col_keys = c("Stakeholder", "Exploration", "Preparation", "Implementation", "Sustainment")) %>%
  merge_v(j = ~Stakeholder) %>%
  width(j= ~Stakeholder, width=1) %>%
  width(j = c(2:5), width=1.3) %>%
  align(j=c(1:5), align="left", part="all") %>%
  hline(i=c(2, 7, 9, 14), border = fp_border()) %>%
  fix_border_issues() %>%
  font(fontname = "Times New Roman", part="all") %>%
  fontsize(size=12, part="all")


read_docx() %>% 
  body_add_flextable(createTable, align="left") %>%
  print(target=here("output", "pcd_table1.docx"))

  

rm(specialProjectsCSPI_2019, publicEducationCSPI_2019)
rm(acts, actsPerYear, legislativeDays, workingDays, workingWeeks, dcDays, dcWeeks, dcHoursPerWeek, dcAnnualTotalHours, dcLegislativeTime, 
   dcAnnualLegislativeHours, districtWeeks, districtHoursPerWeek, districtAnnualTotalHours, districtLegislativeTime, 
   districtAnnualLegislativeHours, totalAnnualTime, totalAnnualLegislativeHours, proportionLegislativeTime, 
   legislativeOutlays_2019, legislativeOutlays_2021, actsPerLegislativeSession, proportionOfOutlaysOnLegislativeActivities)
rm(collectTheRevenueTTB_2019, ssbTaxCollection)
rm(lobbying2009)





## @knitr blah
# BLAH --------------------------------------------------------------------


           # , 
           # 
           # label = paste0(dollars, " (", action, ")")) 
           # 
           # 
           # 
  # %>%
  #   
  #   group_by(stakeholder, phasef) %>%
  #   
  #   summarise(labels = paste(label, collapse="\n"), 
  #             .groups="drop")
  # 
  # 
  # 
  # 
  # ggplot(data=nicer, aes(x=phasef, y=fct_rev(stakeholder))) + 
  #   geom_label(aes(label=labels, hjust=0), 
  #              show.legend = FALSE, 
  #              nudge_x=-0.25, size=3) + 
  #   scale_x_discrete(position="top") +
  #   theme_bw() + 
  #   xlab("") + ylab("") + 
  #   theme(axis.text = element_text(size=10, colour="black"), 
  #         axis.ticks = element_blank(), 
  #         panel.grid = element_blank()) 
  # # caption: All costs displayed are in 2021 dollars. With the exception of the tax revenue gained by the Internal Revenue Service, all costs are expenditures.
  # 
  # ggsave(here("output", "figure_xyz.png"), plot=last_plot(), device="png", width = 10, height=5, units="in")
  # 
  # 
  # nicer %>%
  #   pivot_wider(id_cols = "stakeholder", 
  #               names_from = "phasef", 
  #               values_from = "label", 
  #               values_fn = paste) %>%
  #   gt(rowname_col = "stakeholder")
  # 
    