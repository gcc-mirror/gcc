/* { dg-do assemble } */

#include <arm_neon.h>

poly8_t
test_vget_lane_p8_before (poly8x8_t in)
{
  /* { dg-error "lane -1 out of range 0 - 7" "" {target *-*-*} 0 } */
  return vget_lane_p8 (in, -1);
}

poly8_t
test_vget_lane_p8_beyond (poly8x8_t in)
{
  /* { dg-error "lane 8 out of range 0 - 7" "" {target *-*-*} 0 } */
  return vget_lane_p8 (in, 8);
}
