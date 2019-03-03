library(tidyverse)
library(gridExtra)

iplballs <- read_csv("ipldat2.csv")
iplmatches <- read_csv("iplmatches.csv")



iplmat <- select(iplmatches, id, season)

colnames(iplmat)[1] <- "match_id"

str(ipldat)



alldat1 <- left_join(iplballs, iplmat, by = "match_id")

str(alldat1)


creatdatayears <- function(x, y){
  
dismiss <- alldat1 %>% filter(season > x) %>%
                         filter(season < y) %>%
                          select(player_dismissed) %>%
                        group_by(player_dismissed) %>%
                          summarise(totw = n())

colnames(dismiss)[1] <- "batsman"

totrun <- alldat1 %>% filter(season > x) %>%
                      filter(season < y) %>%
                      group_by(batsman) %>%
                      summarise(tot = sum(batsman_runs))

ave_kpi <- totrun %>% left_join(dismiss, by = "batsman") %>%
                        mutate(average = tot / totw)


totstrik <- alldat1 %>% filter(season > x) %>%
  filter(season < y) %>%
  group_by(batsman) %>%
  summarise(tot = sum(batsman_runs), n = n())
    
  
strik_kpi <- totstrik %>% left_join(dismiss, by = "batsman" ) %>%
                              mutate(strikerate= (tot / n)* 100 , ballpinn = n / totw)  
  

boun <- alldat1 %>% mutate(four = if_else(batsman_runs == 4, 1, 0), six = if_else(batsman_runs == 6, 1, 0)) %>%
                      filter(season > x) %>%
                           filter(season < y) %>%
                              group_by(batsman) %>%
                                summarise(n = n(), tot4 = sum(four), tot6 = sum(six)) %>%
                                  mutate(frate = (tot4/n)*100, sxrate = (tot6/n)*100)


boun2 <- alldat1 %>% mutate(boundrun = if_else(batsman_runs == 4, 4, if_else(batsman_runs == 6, 6, 0)),
                                               bounno = if_else(batsman_runs == 4, 1, if_else(batsman_runs == 6, 1, 0))) %>%
                           filter(season > x) %>%
                              filter(season < y) %>%
                                   group_by(batsman) %>%
                                     summarise(totrun = sum(batsman_runs), bounrun = sum(boundrun), n = n(), bounno2 = sum(bounno)) %>%
                                          mutate(bounper = (bounrun /totrun)*100, nonbounsr = ((totrun - bounrun) / (n - bounno2))*100 )
                           

alldat2 <- alldat1 %>% filter(season > x) %>%
                              filter(season < y) %>%
                                group_by(batsman, match_id) %>%
                                  mutate(ball2 = 1:n()) %>%
                                    filter(batsman_runs %in% c(4,6)) %>%
                                      mutate(rank = rank(ball2)) %>%
                                        ungroup() %>%
                                          group_by(batsman) %>%
                                        filter(rank == 1) %>%
                                            summarise(avefb = mean(ball2, na.rm =T))

dismis2 <- alldat1 %>% filter(season > x) %>%
                           filter(season < y) %>%
                              select(player_dismissed, dismissal_kind) %>%
                                mutate(dismissal = if_else(dismissal_kind == "caught", "caught", if_else(dismissal_kind == "bowled", "bowled", 
                                                                                                         if_else(dismissal_kind == "caught and bowled", "caught", "other")))) %>%

                                  group_by(player_dismissed, dismissal) %>%
                                    summarise(tot = n()) %>%
                                    spread(dismissal, tot) %>%
                                        mutate_all(funs(replace(., is.na(.), 0))) %>%
                                      mutate(bowlper = bowled / (bowled + caught + other) * 100, caughtper = caught/ (bowled + caught + other) * 100, 
                                             otherper = other/(bowled + caught + other) * 100) %>%
                            select(player_dismissed, bowlper, caughtper, otherper)


colnames(dismis2)[1] <- "batsman"


fulldat <- ave_kpi %>% left_join(strik_kpi, by = "batsman" ) %>%
                            left_join(boun, by = "batsman") %>%
                             left_join(boun2, by = "batsman") %>%
                               left_join(alldat2, by = "batsman") %>%
                                 left_join(dismis2, by = "batsman") %>%
                                    select(batsman, average, strikerate, ballpinn, frate, sxrate, bounper, nonbounsr, avefb,  bowlper, caughtper, otherper) %>%
                                      gather("kpi", "value", -batsman)

return(fulldat)

}


allyears <- creatdata(2008, 2019)

ggplot(allyears, aes(x = kpi, y = value)) + geom_point(alpha = 0.5, position = "jitter") +
  
                       facet_wrap(~kpi, scales = "free") +
                              theme(strip.text = element_blank()) 




virat <- filter(allyears, batsman == "V Kohli")
gayle <- filter(allyears, batsman == "CH Gayle")
################### visualise



graph1 <- function(x,z,t,c,v,b,n,m,a,s,d,f,qq, pp){
  
  ggplot(x, aes(x = kpi, y = value)) +
    geom_rect(ymin = z, ymax = t, xmin = 0, xmax = 2, fill = 'lightgrey', col = NA) +
    geom_rect(ymin = t, ymax = c, xmin = 0, xmax = 2, fill = 'white', col = NA) +
    geom_rect(ymin = c, ymax = v, xmin = 0, xmax = 2, fill = 'lightgrey', col = NA) +
    geom_rect(ymin = v, ymax = b, xmin = 0, xmax = 2, fill = 'white', col = NA) +
    geom_rect(ymin = b, ymax = n, xmin = 0, xmax = 2, fill = 'lightgrey', col = NA) +
    geom_rect(ymin = n, ymax = m, xmin = 0, xmax = 2, fill = 'white', col = NA) +
    geom_rect(ymin = m, ymax = a, xmin = 0, xmax = 2, fill = 'lightgrey', col = NA) +
    geom_rect(ymin = a, ymax = s, xmin = 0, xmax = 2, fill = 'white', col = NA) +
    geom_rect(ymin = s, ymax = d, xmin = 0, xmax = 2, fill = 'lightgrey', col = NA) +
    geom_rect(ymin = d, ymax = f, xmin = 0, xmax = 2, fill = 'white', col = NA) +  
    annotate("text", x = 1, y = c, label = c, fontface =2) + 
    annotate("text", x = 1, y = b, label = b, fontface =2) + 
    annotate("text", x = 1, y = m, label = m, fontface =2) + 
    annotate("text", x = 1, y = s, label = s, fontface =2) + 
    annotate("text", x = 1, y = f, label = f, fontface =2) + 
    geom_point(size = 5, alpha = 0.5, col = "steelblue") + 
    labs(title = qq) +
    guides(col = FALSE) +
    ylim(z, f) +        
    theme(panel.background = element_blank(), strip.text = element_blank(), 
          axis.text.y = element_blank(), axis.ticks.y = element_blank(), axis.title.y = element_blank(), 
          axis.text.x = element_blank(), plot.title = element_text(hjust = 0.5, size = 8), 
          plot.margin = unit(c(0.5, pp, 0.5,pp), "cm"), axis.title.x = element_blank(), axis.ticks.x = element_blank())
  
}



graphper <- function(x,z,c,v,b,n,m,a,s,d,f,g,j,l,w,r, y, ww,pp){
  
  ggplot(x, aes(x = kpi, y = value)) + 
  geom_rect(ymin = z, ymax = c, xmin = 0, xmax = 2, fill = 'lightgrey', col = NA) +
  geom_rect(ymin = c, ymax = v, xmin = 0, xmax = 2, fill = 'white', col = NA) +
  geom_rect(ymin = v, ymax = b, xmin = 0, xmax = 2, fill = 'lightgrey', col = NA) +
  geom_rect(ymin = b, ymax = n, xmin = 0, xmax = 2, fill = 'white', col = NA) +
  geom_rect(ymin = n, ymax = m, xmin = 0, xmax = 2, fill = 'lightgrey', col = NA) +
  geom_rect(ymin = m, ymax = a, xmin = 0, xmax = 2, fill = 'white', col = NA) +
  geom_rect(ymin = a, ymax = s, xmin = 0, xmax = 2, fill = 'lightgrey', col = NA) +
  geom_rect(ymin = s, ymax = d, xmin = 0, xmax = 2, fill = 'white', col = NA) +
  geom_rect(ymin = d, ymax = f, xmin = 0, xmax = 2, fill = 'lightgrey', col = NA) +
  geom_rect(ymin = f, ymax = g, xmin = 0, xmax = 2, fill = 'white', col = NA) +
  annotate("text", x = 1, y = v, label = j, fontface =2)+  #20
  annotate("text", x = 1, y = n, label = l, fontface =2) + #40
  annotate("text", x = 1, y = a, label = w, fontface =2) + #60
  annotate("text", x = 1, y = d, label = r, fontface =2) + #8
  annotate("text", x = 1, y = g, label = y, fontface =2) +  #100
  geom_point(size = 5, alpha = 0.5, col = "steelblue") + 
  labs(title = ww) +
  guides(col = FALSE) +
  ylim(z, g) +        
  theme(panel.background = element_blank(), strip.text = element_blank(), 
        axis.text.y = element_blank(), axis.ticks.y = element_blank(), axis.title.y = element_blank(), 
        axis.text.x = element_blank(), plot.title = element_text(hjust = 0.5, size = 8),plot.margin = unit(c(0.5, pp, 0.5,pp), "cm"),
        axis.title.x = element_blank(), axis.ticks.x = element_blank())

}



theplot <- function(x,c,v,b,n,m,a,s,d,f,g,pp){

batave <- graph1(x,0,4.5,9,13.5,18,22.5,27,31.5,36,40.5,45, "Batting \n Average", -0.5)
strikerat <- graph1(c, 90,97,104,111,118,125,132, 139, 146,153,160, "Strike \nRate", -0.5)
ballboun <- graph1(v,0,0.8,1.6,2.4,3.2,4,4.8,5.6,6.4,7.2,8, "First \n Boundary", -0.5)
ballinn <- graph1(b,0,4,8,12,16,20,24,28,32,36,40, "Balls \n per inn", -0.5 )
boundper <- graphper(n, 0,10,20,30,40,50,60,70,80,90,100, "20 %", "40 %", "60 %", "80 %", "100%","Boundary \n Percent", -0.5)
bowlper <- graphper(m, 0,10,20,30,40,50,60,70,80,90,100,"20 %", "40 %", "60 %", "80 %", "100%","Bowled \nPercent", -0.5)
caughtper <- graphper(a, 0,10,20,30,40,50,60,70,80,90,100,"20 %", "40 %", "60 %", "80 %", "100%","Caught \nPercent", -0.5)
fourper <- graphper(s, 0,5,10,15,20,25,30,35,40,45,50, "10 %", "20 %", "30 %", "40 %", "50 %","Four \nRate", -0.5)
nonboundsr <- graph1(d,0,10,20,30,40,50,60,70,80,90,100, "Non \n Bound SR", -0.5 )
OtherW <- graphper(f, 0,10,20,30,40,50,60,70,80,90,100,"20 %", "40 %", "60 %", "80 %", "100%","Other \n Wicket Per", -0.5)
sixper <- graphper(g, 0,2,4,6,8,10,12,14,16,18,20, "4 %", "8 %", "12 %", "16 %", "20 %","Six \n Rate", -0.5)
 


p1 <- grid.arrange(batave, strikerat,nonboundsr,ballinn, ballboun, boundper, fourper,sixper, bowlper, caughtper,OtherW,   ncol = 11, top = pp)
 

return(p1)

}

theplot((filter(virat, kpi == "average")), filter(virat, kpi == "strikerate"), filter(virat, kpi == "avefb"), filter(virat, kpi == "ballpinn"), 
        filter(virat, kpi == "bounper"), filter(virat, kpi == "bowlper"), filter(virat, kpi == "caughtper"), filter(virat, kpi == "frate"),
        filter(virat, kpi == "nonbounsr"), filter(virat, kpi == "otherper"), filter(virat, kpi == "sxrate"), "Virat Kohli All Time IPL")


theplot((filter(gayle, kpi == "average")), filter(gayle, kpi == "strikerate"), filter(gayle, kpi == "avefb"), filter(gayle, kpi == "ballpinn"), 
        filter(gayle, kpi == "bounper"), filter(gayle, kpi == "bowlper"), filter(gayle, kpi == "caughtper"), filter(gayle, kpi == "frate"),
        filter(gayle, kpi == "nonbounsr"), filter(gayle, kpi == "otherper"), filter(gayle, kpi == "sxrate"), "Chris Gayle All Time IPL")
