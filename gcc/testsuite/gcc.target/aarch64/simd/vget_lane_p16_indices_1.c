/* { dg-do assemble } */

#include <arm_neon.h>

poly16_t
test_vget_lane_p16_before (poly16x4_t in)
{
  /* { dg-error "lane -1 out of range 0 - 3" "" {target *-*-*} 0 } */
  return vget_lane_p16 (in, -1);
}

poly16_t
test_vget_lane_p16_beyond (poly16x4_t in)
{
  /* { dg-error "lane 4 out of range 0 - 3" "" {target *-*-*} 0 } */
  return vget_lane_p16 (in, 4);
}
