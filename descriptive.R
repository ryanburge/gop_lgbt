
sum_lgbt <- cces16 %>% 
  filter(sexuality == 2 | sexuality ==3 | sexuality ==4 | sexuality ==5 | trans ==1) %>% 
  filter(faminc < 17) %>% 
  mutate(age = 2017- birthyr) %>% 
  mutate(male = recode(gender, "1=1; else=0")) %>% 
  mutate(white = recode(race, "1=1; else=0")) %>% 
  select(`Mean Age` = age, `Mean Education` = educ, `Mean Household Income` = faminc) %>% 
  summarise_all(funs(mean)) %>% 
  gather(key, value, everything()) %>% 
  separate(key, into = c("variable", "stat"), sep = "_") %>%
  spread(stat, value) %>%
  mutate_each(funs(round(., 1)), -variable) %>% rename("LGBT" = `<NA>`)


sum_all <- cces16 %>% 
  # filter(sexuality == 2 | sexuality ==3 | sexuality ==4 | sexuality ==5 | trans ==1) %>% 
  filter(faminc < 17) %>% 
  mutate(age = 2017- birthyr) %>% 
  mutate(male = recode(gender, "1=1; else=0")) %>% 
  mutate(white = recode(race, "1=1; else=0")) %>% 
  select(`Mean Age` = age, `Mean Education` = educ, `Mean Household Income` = faminc) %>% 
  summarise_all(funs(mean)) %>% 
  gather(key, value, everything()) %>% 
  separate(key, into = c("variable", "stat"), sep = "_") %>%
  spread(stat, value) %>%
  mutate_each(funs(round(., 1)), -variable) %>% rename("Entire Sample" = `<NA>`)

s <- bind_cols(sum_all, sum_lgbt) %>% select(-variable1) 



gender_lgbt <- cces16 %>% 
  filter(sexuality == 2 | sexuality ==3 | sexuality ==4 | sexuality ==5 | trans ==1) %>% 
  count(gender, wt = commonweight_vv) %>% 
  mutate(pct = prop.table(n)) %>% 
  mutate(pct = pct*100) %>% 
  mutate(gender = to_factor(gender)) %>% 
  mutate(pct = round(pct, 2)) %>% 
  select(-n) %>% 
  filter(gender == "Male") %>% 
  mutate(gender = recode(gender, "'Male' = 'Male (%)'"))

gender_all <- cces16 %>% 
  # filter(sexuality == 2 | sexuality ==3 | sexuality ==4 | sexuality ==5 | trans ==1) %>% 
  count(gender, wt = commonweight_vv) %>% 
  mutate(pct = prop.table(n)) %>% 
  mutate(pct = pct*100) %>% 
  mutate(gender = to_factor(gender)) %>% 
  mutate(pct = round(pct, 2)) %>% 
  select(-n) %>% 
  filter(gender == "Male") %>% 
  mutate(gender = recode(gender, "'Male' = 'Male (%)'"))

g <- bind_cols(gender_all, gender_lgbt) %>% select(-gender1) %>% rename("Entire Sample" = pct, "LGBT" = pct1, variable = gender)



  
race_lgbt <- cces16 %>% 
  filter(sexuality == 2 | sexuality ==3 | sexuality ==4 | sexuality ==5 | trans ==1) %>% 
  count(race, wt = commonweight_vv) %>% 
  mutate(pct = prop.table(n)) %>% 
  mutate(pct = pct*100) %>% 
  mutate(race = to_factor(race)) %>% 
  mutate(pct = round(pct, 2)) %>% 
  select(-n) %>% 
  filter(race == "White" | race == "Black" | race == "Hispanic" | race == "Asian" | race == "Mixed") %>% 
  mutate(race = recode(race, "'White' = 'White (%)';'Black' = 'Black (%)';'Hispanic' = 'Hispanic (%)';'Asian' = 'Asian (%)';'Mixed' = 'Mixed (%)'"))

race_all <- cces16 %>% 
  # filter(sexuality == 2 | sexuality ==3 | sexuality ==4 | sexuality ==5 | trans ==1) %>% 
  count(race, wt = commonweight_vv) %>% 
  mutate(pct = prop.table(n)) %>% 
  mutate(pct = pct*100) %>% 
  mutate(race = to_factor(race)) %>% 
  mutate(pct = round(pct, 2)) %>% 
  select(-n) %>% 
  filter(race == "White" | race == "Black" | race == "Hispanic" | race == "Asian" | race == "Mixed") %>% 
  mutate(race = recode(race, "'White' = 'White (%)';'Black' = 'Black (%)';'Hispanic' = 'Hispanic (%)';'Asian' = 'Asian (%)';'Mixed' = 'Mixed (%)'")) 


r <- bind_cols(race_all, race_lgbt) %>% select(-race1) %>% rename("Entire Sample" = pct, "LGBT" = pct1, variable = race)

table <- bind_rows(s,g,r)

write.table(table, file = "descriptive.txt", sep = ",", quote = FALSE, row.names = F)

