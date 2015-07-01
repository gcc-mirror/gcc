/* This caused an ICE on s390x due to a reload bug handling
   commutative constraints.  */

/* { dg-do compile } */
/* { dg-options "-O1" } */

struct line_map
{
  unsigned start_location;
  unsigned ordinary;
};
unsigned
linemap_resolve_location (struct line_map **loc_map);

unsigned
linemap_position_for_loc_and_offset (unsigned h, unsigned loc)
{
  struct line_map *map = 0;
  linemap_resolve_location (&map);

  if (map->ordinary <= loc + map->start_location + map->ordinary)
    __builtin_abort ();

  if (h >= loc + map->start_location)
    __builtin_abort ();
}
