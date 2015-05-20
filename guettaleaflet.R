# Téléchargement des données
guetta <- url(description = "http://dansmonlabo.com/files/pays_v3.csv")
ctry <- read.csv(guetta)

# Import du fond de carte
load('world.RData')

# Nettoyage des données
## Sélection de certaines colonnes
ctry <- ctry[,c("code_pays_iso_a3","count", "PIB_2013_1000_no_blank",
                "geo_longitude",  "geo_latitude" )]
## Correction de la localisation du Vietnam
ctry[ctry$code_pays_iso_a3 == "VNM", 
     c("geo_longitude",  "geo_latitude")] <- c(107.8455,12.79458)
## Jointure avec le fond de carte pour obtenir les nom de pays
ctry <- merge(ctry, world@data[,c(2:3)], 
              by.x = "code_pays_iso_a3", by.y = "ISO3", all.x = T)
## Suppression des lignes non renseignées
ctry <- ctry[!is.na(ctry$geo_longitude),]
## Sélection des pays cités plus d'une fois
ctry <- ctry[ctry$count > 1,]

# Préparation des données 
## Tri du data frame de manière à avoir les cercles plus gros en dessous.
ctry <- ctry[order(ctry$count, decreasing = T),]
## Taille des rayons des cercles à afficher (en pixels)
ctry$size <- sqrt(ctry$count*4 / pi)
## Label des cercles (ce qui apparait quand on click dessus)
ctry$label <- paste("<b>", ctry$name, "</b> <br>Pays cité dans ", 
                    ctry$count, "chroniques.")

# Construction de la carte
library(leaflet)
## Initialisation 
m <- leaflet(padding = 0)
## Ajout des pays
m <- addPolygons(map = m, data = world, opacity = 100, 
                 color = "#FAFCFA", 
                 weight = 0.25,popup = NULL,
                 options = list(clickable = FALSE), 
                 fill = T, fillColor = "#B3C4B3", 
                 fillOpacity = 100)
## Ajout des cercles
m <- addCircleMarkers(map = m, 
                      lng = ctry$geo_longitude, 
                      lat = ctry$geo_latitude, 
                      radius = ctry$size, weight = 0.25, 
                      stroke = T, opacity = 100,
                      fill = T, fillColor = "#920000", 
                      fillOpacity = 100,
                      popup = ctry$label,
                      color = "white")
## Centrage de la carte 
m <- fitBounds(map = m, 
               lng1 = min(ctry$geo_longitude),
               lat1 = min(ctry$geo_latitude),
               lng2 = max(ctry$geo_longitude), 
               lat2 = max(ctry$geo_latitude))
## Dimensions de la carte
m$width <- 474
m$height <- 200

# Export de la carte en html
library(htmlwidgets)
saveWidget(m, 'map.html', selfcontained = TRUE)
