enum zone_type {
  ZONE_DMA,
  ZONE_NORMAL,
  ZONE_MOVABLE,
  MAX_NR_ZONES
};
static unsigned long arch_zone_lowest_possible_pfn[MAX_NR_ZONES];
static unsigned long arch_zone_highest_possible_pfn[MAX_NR_ZONES];
void free_area_init_nodes(unsigned long *max_zone_pfn)
{
  enum zone_type i;
  for (i = 1; i < MAX_NR_ZONES; i++)
  {
    if (i == ZONE_MOVABLE)
      continue;
    unsigned long _x = max_zone_pfn[i];
    unsigned long _y = arch_zone_lowest_possible_pfn[i];
    arch_zone_highest_possible_pfn[i] = _x > _y ? _x : _y;
  }
}
