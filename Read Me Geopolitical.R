# ============================================================
# NIGERIA GEOPOLITICAL ZONES MAP
# ============================================================

# Install packages (run once)
# install.packages(c("sf", "dplyr", "ggplot2"))

# Load packages
library(sf)
library(dplyr)
library(ggplot2)

# ============================================================
# STEP 1: READ STATE SHAPEFILE
# ============================================================

states <- st_read("NGA_adm1.shp")

# ============================================================
# STEP 2: REMOVE NON-STATE FEATURE
# ============================================================

states <- states %>%
  filter(NAME_1 != "Water body")

# ============================================================
# STEP 3: CREATE GEOPOLITICAL ZONE LOOKUP TABLE
# ============================================================

zone_lookup <- data.frame(
  NAME_1 = c(
    
    # South East
    "Abia",
    "Anambra",
    "Ebonyi",
    "Enugu",
    "Imo",
    
    # South South
    "Akwa Ibom",
    "Bayelsa",
    "Cross River",
    "Delta",
    "Edo",
    "Rivers",
    
    # South West
    "Ekiti",
    "Lagos",
    "Ogun",
    "Ondo",
    "Osun",
    "Oyo",
    
    # North Central
    "Benue",
    "Kogi",
    "Kwara",
    "Nassarawa",
    "Niger",
    "Plateau",
    "Federal Capital Territory",
    
    # North East
    "Adamawa",
    "Bauchi",
    "Borno",
    "Gombe",
    "Taraba",
    "Yobe",
    
    # North West
    "Jigawa",
    "Kaduna",
    "Kano",
    "Katsina",
    "Kebbi",
    "Sokoto",
    "Zamfara"
  ),
  
  zone = c(
    rep("South East", 5),
    rep("South South", 6),
    rep("South West", 6),
    rep("North Central", 7),
    rep("North East", 6),
    rep("North West", 7)
  )
)

# ============================================================
# STEP 4: JOIN STATES TO GEOPOLITICAL ZONES
# ============================================================

states_zone <- states %>%
  left_join(zone_lookup, by = "NAME_1")

# ============================================================
# STEP 5: CHECK FOR UNMATCHED STATES
# ============================================================

states_zone %>%
  filter(is.na(zone)) %>%
  select(NAME_1)

# Should return 0 rows

# ============================================================
# STEP 6: DISSOLVE STATES INTO GEOPOLITICAL ZONES
# ============================================================

zones <- states_zone %>%
  group_by(zone) %>%
  summarise()

# ============================================================
# STEP 7: VERIFY OUTPUT
# ============================================================

print(nrow(zones))      # Should be 6

print(zones$zone)

# ============================================================
# STEP 8: GENERATE GEOPOLITICAL MAP
# ============================================================

zone_map <- ggplot(zones) +
  
  geom_sf(
    aes(fill = zone),
    color = "black",
    linewidth = 0.5
  ) +
  
  geom_sf_text(
    aes(label = zone),
    size = 3.5,
    fontface = "bold"
  ) +
  
  labs(
    title = "Nigeria Geopolitical Zones",
    subtitle = "Derived from State Administrative Boundaries",
    fill = "Zone"
  ) +
  
  theme_minimal() +
  
  theme(
    plot.title = element_text(
      hjust = 0.5,
      face = "bold",
      size = 16
    ),
    
    plot.subtitle = element_text(
      hjust = 0.5,
      size = 12
    ),
    
    legend.position = "right",
    
    axis.text = element_blank(),
    axis.ticks = element_blank()
  )

# Display map
zone_map

# ============================================================
# STEP 9: SAVE MAP
# ============================================================

ggsave(
  filename = "Nigeria_Geopolitical_Zones_Map.png",
  plot = zone_map,
  width = 10,
  height = 8,
  dpi = 300
)
