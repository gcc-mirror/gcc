// { dg-do compile }
// { dg-additional-options "-fno-exceptions --param=logical-op-non-short-circuit=0" }

typedef unsigned int location_t;
const location_t MAX_LOCATION_T = 0x7FFFFFFF;
struct line_maps {
  unsigned int  info_ordinary;
  location_t *maps;
  unsigned int used;
  location_t *data;
};
inline location_t LINEMAPS_MACRO_LOWEST_LOCATION(const line_maps *set) {
  return set->used
             ? set->maps[set->used - 1]
             : MAX_LOCATION_T + 1;
}
const location_t *linemap_lookup(const line_maps *set, location_t line) {
  int mn = set->info_ordinary;
  if (mn >= 0)
  if ((unsigned int)mn < set->used)
  return &set->maps[0];
  __builtin_unreachable();
}
bool linemap_location_from_macro_expansion_p(const class line_maps *set,
                                             location_t location) {
  if (location > MAX_LOCATION_T)
    location = set->data[location & MAX_LOCATION_T];
  return location >= LINEMAPS_MACRO_LOWEST_LOCATION(set);
}
void first_map_in_common_1(line_maps *set, location_t *loc0,
                                             location_t *loc1) {
  linemap_lookup(set, 0);
  __builtin_unreachable();
}
int linemap_compare_locations(line_maps *set, location_t pre, location_t post) {
  bool pre_virtual_p;
  location_t l0 = pre, l1 = post;
  if (l0 > MAX_LOCATION_T)
    l0 = set->data[l0 & MAX_LOCATION_T];
  if (l1 > MAX_LOCATION_T)
    l1 = set->data[l1 & MAX_LOCATION_T];;
  if (l0 == l1)
    return 0;
  if ((pre_virtual_p = linemap_location_from_macro_expansion_p(set, l0)))
    l0 = set->data[l0 & MAX_LOCATION_T];
  if (linemap_location_from_macro_expansion_p(set, l1))
    l1 = set->data[l1 & MAX_LOCATION_T];
  if (l0 == l1)
     if (pre_virtual_p)
	first_map_in_common_1(set, &l0, &l1);
  if (l0 > MAX_LOCATION_T)
    if (l1 > MAX_LOCATION_T)
      l1 = set->data[l1 & MAX_LOCATION_T];
  return l1 - l0;
}
