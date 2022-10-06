library(readODS)
library(rio)
library(tidyverse)
library(forcats)
library(highcharter)
library(sp)
library(readxl)

subv <- read_ods("I:/SUPPORT/04_STATS/subventions/synthèse subv.ods")
foot <- import_list("I:/SUPPORT/04_STATS/subventions/Emploi Foot.xls")
pro <- import_list("I:/SUPPORT/04_STATS/subventions/Professionnalisation 2020 2021 2022.xls")

load("I:/SUPPORT/05_CARTO/Fonds de cartes/cartes.RData")
load("I:/SUPPORT/05_CARTO/Fonds de cartes/basecom.RData")
rm(bvwgs,epciwgs,densitewgs,densiteBFC,depwgs,regwgs,ZRR)

subvention <- subv %>% mutate(INSEE_COM=as.character(commune)) %>% select(INSEE_COM,perimetre=`echelon/structure`,montant,enveloppe,structure,sport,club,sigle,année) %>%
  left_join(.,basecomQPV %>% select(INSEE_COM=CODGEO,LIBGEO,EPCI,BV2012,DEP,REG,P19_POP), by="INSEE_COM")

subv_foot <- foot[["2020"]] %>% select(INSEE_COM=`Code Insee`,perimetre= `Champ d'action territorial` ,montant = Accordé,
                                       financement= `Sous-Type financement`) %>%
  mutate(année="2020") %>%
  bind_rows(foot[["2021"]] %>%select(INSEE_COM=`Code Insee`,perimetre= `Champ d'action territorial` ,
                                     montant = Accordé, financement= `Sous-Type financement`) %>%
              mutate(année="2021") ) %>%
  bind_rows(foot[["2022"]] %>%select(INSEE_COM=`Code Insee`,perimetre= `Champ d'action territorial` ,
                                     montant = Accordé, financement= `Sous-Type financement`) %>%
              mutate(année="2022") ) %>%
  left_join(.,basecomQPV %>% select(INSEE_COM=CODGEO,LIBGEO,EPCI,BV2012,DEP,REG,P19_POP), by="INSEE_COM")


subv_pro <- pro[["2020"]] %>% select(INSEE_COM=`Code Insee`,perimetre= `Champ d'action territorial` ,montant = Accordé,
                                     financement= `Sous-Type financement`) %>%
  mutate(année="2020") %>%
  bind_rows(pro[["2021"]] %>%select(INSEE_COM=`Code Insee`,perimetre= `Champ d'action territorial` ,montant = Accordé,
                                    financement= `Sous-Type financement`) %>%
              mutate(année="2021") ) %>%
  bind_rows(pro[["2022"]] %>%select(INSEE_COM=`Code Insee`,perimetre= `Champ Action Territorial` ,montant = Accordé,
                                    financement= `Sous-Type financement`) %>%
              mutate(année="2022") ) %>%
  left_join(.,basecomQPV %>% select(INSEE_COM=CODGEO,LIBGEO,EPCI,BV2012,DEP,REG,P19_POP), by="INSEE_COM")



######## subv par département



### subv foot
gfoot <- map(c("2022":"2020"), function(x){
  
  
  hchart(subv_foot %>%
           filter(!(DEP %in% "40") & 
                    année== x ) %>%
           group_by(DEP,perimetre) %>%
           summarise(montant=sum(montant,na.rm = F))  ,
         hcaes(  x =   DEP,   y = montant,   group = fct_relevel( perimetre,
                                                                  "Local", "Départemental", "Régional") ),
         type = "column",stacking = "normal",yAxis=0
         #color=c("lightblue","lightgreen","orange")
         
  ) %>%
    
    hc_xAxis(categories=c('21','25','39','58','70','71','89','90'),
             title = list(text = "Départements (couleur : champ d'action territorial)") )  %>%
    
    hc_add_series(basecom %>% 
                    filter(REG=="27") %>%
                    group_by(DEP) %>%
                    summarise(pop=sum(pop)) ,
                  hcaes(x = fct_rev(DEP) ,y = pop),
                  type = "lollipop", color = "Purple", 
                  #maxPointWidth  = 0.2 , 
                  name = "pop 2019", yAxis=1,
    ) %>%
    
    hc_yAxis_multiples(
      list(lineWidth = 3,
           title = list(text = "Montant des subventions à l'emploi"),
           opposite = FALSE) ,
      list(showLastLabel = FALSE, 
           title = list(text = "Pop"),
           opposite = TRUE) ) %>%
    
    hc_title(text = paste("Aides à la professionnalisation accordées au football en ", x , "en BFC") )   %>%
    hc_chart(polar = F)  %>%
    hc_add_theme(hc_theme_smpl())  })

hw_grid(gfoot, ncol=3, rowheight = 500) %>%
  htmltools::browsable()


### subv totales
gprof <- map(c("2022":"2020"), function(x){
  
  
  hchart(subv_pro %>%
           filter(!(DEP %in% "40") & 
                    !is.na(perimetre) &
                    année== x ) %>%
           group_by(DEP,perimetre) %>%
           summarise(montant=sum(montant,na.rm = T))    ,
         hcaes(  x =   DEP,   y = montant,   group = fct_relevel( perimetre,
                                                                  "Local", "Départemental", "Régional", "National", "International") ) ,
         type = "column",stacking = "normal",yAxis=0
         #color=c("lightblue","lightgreen","orange")
         
  ) %>%
    
    hc_xAxis(categories=c('21','25','39','58','70','71','89','90'),
             title = list(text = "Départements (couleur : champ d'action territorial)") )  %>%
    
    hc_add_series(basecom %>% 
                    filter(REG=="27") %>%
                    group_by(DEP) %>%
                    summarise(pop=sum(pop)) ,
                  hcaes(x = fct_rev(DEP) ,y = pop),
                  type = "lollipop", color = "Purple", 
                  #maxPointWidth  = 0.2 , 
                  name = "pop 2019", yAxis=1,
    ) %>%
    
    hc_yAxis_multiples(
      list(lineWidth = 3,
           title = list(text = "Montant des subventions à l'emploi"),
           opposite = FALSE) ,
      list(showLastLabel = FALSE, 
           title = list(text = "Pop"),
           opposite = TRUE) ) %>%
    
    hc_title(text = paste("Aides à la professionnalisation", x, "en BFC" ) )   %>%
    hc_chart(polar = F)  %>%
    hc_add_theme(hc_theme_smpl())  })

hw_grid(gprof, ncol=3, rowheight = 500) %>%
  htmltools::browsable()



### CPJ et TDJ par dept


TDJ <-   hchart(subvention %>%
                  filter(!(DEP %in% "40") & enveloppe %in% c("TDJ") ) %>%
                  #   distinct(INSEE_COM,.keep_all = T) %>%
                  group_by(DEP,perimetre) %>%
                  count() ,
                hcaes(  x = DEP,    y = n,   group = perimetre),
                type = "column",stacking = "normal",
                color=c("gold","yellow") ) %>%
  
  hc_xAxis(categories=c('21','25','39','58','70','71','89','90'),
           title = list(text = "Départements")) %>%
  
  hc_yAxis_multiples(
    list(lineWidth = 3,
         title = list(text = "Nombre de contrats Terres de Jeux"),
         opposite = FALSE) ,
    list(showLastLabel = FALSE, 
         title = list(text = "Pop"),
         opposite = TRUE) ) %>%    
  hc_add_series( subvention %>%
                   filter(!(DEP %in% "40") & enveloppe  == "TDJ" & perimetre == "Commune" ) %>%
                   # distinct(INSEE_COM,.keep_all = T) %>%
                   group_by(DEP) %>%
                   summarise(pop=sum(P19_POP)) ,
                 hcaes(  x = DEP,    y = pop),
                 type = "lollipop",stacking = "normal",color="Orange",
                 name="population des communes TDJ",yAxis=1 ) %>%
  
  
  hc_add_series(basecom %>% 
                  filter(REG=="27" & 
                           EPCI %in% subvention$EPCI[subvention$perimetre=="EPCI" & subvention$enveloppe=="TDJ"]
                  ) %>%
                  group_by(DEP) %>%
                  summarise(
                    popepci=sum(pop)) ,
                hcaes(x = DEP ,y = popepci ),
                type = "lollipop", color = "Tomato", 
                stacking="normal",
                name = "population des EPCI TDJ",
                yAxis=1
  )  %>%
  hc_add_series(basecom %>% 
                  filter(REG=="27" ) %>%
                  group_by(DEP) %>%
                  summarise(
                    popepci=sum(pop)) ,
                hcaes(x = DEP ,y = popepci ),
                type = "lollipop", color = "Purple", 
                stacking="normal",
                name = "population totale",
                yAxis=1
  )  %>%
  hc_chart(polar = F) 
TDJ

CPJ <- hchart(subvention %>%
                filter(!(DEP %in% "40") & enveloppe %in% c("CPJ") ) %>%
                #     distinct(INSEE_COM,.keep_all = T) %>%
                group_by(DEP) %>%
                count() ,
              hcaes(  x = DEP,    y = n),
              type = "column",stacking = "normal",
              color = "lightgreen",
              name = "nombre d'équipements CPJ") %>%
  
  hc_xAxis(categories=c('21','25','39','58','70','71','89','90'),
           title = list(text = "Départements")) %>%
  
  hc_yAxis_multiples(
    list(lineWidth = 3,
         title = list(text = "Montant d'équipements Centres de Préparation aux Jeux"),
         opposite = FALSE) ,
    list(showLastLabel = FALSE, 
         title = list(text = "Nombre de communes concernées"),
         opposite = TRUE) ) %>%
  
  hc_add_series(subvention %>%
                  filter(!(DEP %in% "40") & enveloppe %in% c("CPJ") ) %>%
                  distinct(INSEE_COM,.keep_all = T) %>%
                  group_by(DEP) %>%
                  count() ,
                hcaes(  x = DEP,    y = n),
                type = "lollipop",stacking = "normal",
                name='nombre de communes concernées',
                yAxis=1
  )  %>%
  hc_chart(polar = F) 
CPJ

PEP <-   hchart(subvention %>%
                  filter(!(DEP %in% "40") & enveloppe %in% c("PEP vague 2","PEP vague 1") ) %>%
                  group_by(DEP,enveloppe) %>%
                  summarise(montant=sum(montant,na.rm = F)) ,
                hcaes(  x = DEP,    y = montant,   group = fct_rev(enveloppe) ),
                type = "column",stacking = "normal",
                color=c("Tomato","orange") ) %>%
  
  hc_xAxis(categories=c('21','25','39','58','70','71','89','90'),
           title = list(text = "Départements")) %>%
  
  hc_yAxis_multiples(
    list(lineWidth = 3,
         title = list(text = "Montant des subventions équipements"),
         opposite = FALSE) ,
    list(showLastLabel = FALSE, 
         title = list(text = "Populations"),
         opposite = TRUE) ) %>%
  
  hc_add_series(basecom %>% 
                  filter(REG=="27") %>%
                  group_by(DEP) %>%
                  summarise(pop=sum(pop)) ,
                hcaes(x = DEP ,y = pop ),
                type = "lollipop", color = "Purple", 
                #maxPointWidth  = 0.2 , 
                name = "pop 2019",
                yAxis=1
  )  %>%
  #hc_yAxis(type="logarithmic") %>%
  hc_chart(polar = F) 
PEP  


hw_grid(gfoot,gprof, TDJ,CPJ, PEP, ncol=3, rowheight = 500) %>%
  htmltools::browsable()






subvention %>%
  filter(!(DEP %in% "40")) %>%
  group_by(DEP,enveloppe,année) %>%
  summarise(montant=sum(montant,na.rm = T)) %>%
  arrange(montant) %>%
  ggplot() +
  
  aes(
    x = fct_rev(DEP),
    y = montant,
    fill = enveloppe,
    colour = enveloppe
  ) +
  geom_col( width = 0.9) +
  
  geom_col(data= basecom %>% 
             filter(REG=="27") %>%
             group_by(DEP) %>%
             summarise(pop=sum(pop)) ,
           inherit.aes = F,
           aes(x= (DEP),
               y=pop),width = 0.2,fill="#aa451150" ) +
  
  coord_flip() +
  #coord_polar()+
  theme_minimal() +
  facet_wrap(vars(année)) 

#réduire à une ligne par indicateur : montant agrégés... avant de merger sur couche spatiale

#montants par commune, bv, epci, dep

library(leaflet)
library(leafpop)
library(leaflet.minicharts)
library(rgdal)
library(rgeos)
library(leaflet.providers)
#devtools::install_github("statnmap/HatchedPolygons")
library(HatchedPolygons)
library(leaflegend)

com27centre <- gCentroid( com27wgs,byid=T,id=com27wgs$INSEE_COM )
com27wgs$x <- com27centre@coords[,1]
com27wgs$y <- com27centre@coords[,2]

plot(com27centre)

TDJepci <- subset(epcicarto,CODE_EPCI %in% 
                    subvention$EPCI[subvention$perimetre=="EPCI" & subvention$enveloppe=="TDJ"])
TDJepcih <- hatched.SpatialPolygons(TDJepci,density = 50, angle = 45,fillOddEven=T )

TDJcom <- subset(com27wgs,INSEE_COM %in% 
                   subvention$INSEE_COM[subvention$perimetre=="Commune" & subvention$enveloppe=="TDJ"])
TDJcomh <- hatched.SpatialPolygons(TDJcom,density = 80, angle = 135,fillOddEven=T  )

CPJcarto <- merge(subvention %>% filter(enveloppe=="CPJ"),
                  com27wgs@data %>% select(INSEE_COM,x,y),by="INSEE_COM")


leaflet() %>% addProviderTiles(providers$OpenStreetMap) %>% 
  setView(lng = 5.1, lat = 47.27, zoom = 8) %>%
  addPolygons(data=dep27carto,weight=4,opacity = 0.6,color = "#2F4F4F", fill=F,) %>%
  addPolygons(data=TDJepci,weight=3,opacity = 1,color = "#f4a460",fillColor = "transparent",
              popup=~popupTable(TDJepci@data %>% select(CODE_EPCI,LIBGEO,P19_POP),
                                row.numbers = F,feature.id = F)) %>% 
  addPolylines(data=TDJepcih,color = "#f4a460") %>%
  addPolygons(data=TDJcom,weight=3,opacity = 1,color = "#4682b4",fillColor = "transparent",
              popup=~popupTable(TDJcom@data %>% select(INSEE_COM,NOM,POPULATION),
                                row.numbers = F,feature.id = F)) %>% 
  addPolylines(data=TDJcomh,color = "#4682b4") %>%
  addMarkers(data=CPJcarto,~x,~y,
             popup=~popupTable(CPJcarto %>% select(INSEE_COM,LIBGEO,P19_POP,structure,sport,perimetre,enveloppe),
                               row.numbers = F,feature.id = F),
             clusterOptions = markerClusterOptions(removeOutsideVisibleBounds = F),
             labelOptions = labelOptions(noHide = F,direction = 'auto') ) %>%
  addLegendImage(images = makeSymbol(shape = "circle",width = 30,strokeWidth = 10,
                                     fillColor  = "lightgreen", color = "#adff2f", text="XX")  ,
                 labels = "Centres de Préparation aux Jeux" ,position ="bottomright")  %>%
  addLegend(colors = "#4682b4",labels = "Terres de Jeux : Communes",position = "bottomright") %>%
  addLegend(colors = "#f4a460",labels = "Terres de Jeux : EPCI",position = "bottomright") 


