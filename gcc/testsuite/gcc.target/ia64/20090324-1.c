/* { dg-do compile } */
/* { dg-options "-O3 -fmodulo-sched" } */

static char *place_region_bounds_x, *place_region_bounds_y;
static void read_place () {
  char msg[300];
  update_screen (msg);
}
static void alloc_and_load_placement_structs () {
  int i, j;
  for (j=0;
      j<100;
      j++) {
    place_region_bounds_x[i] = place_region_bounds_x[i-1];
    place_region_bounds_y[i] = place_region_bounds_y[i-1];
  }
}
void place_and_route () {
  read_place ();
  alloc_and_load_placement_structs ();
}
