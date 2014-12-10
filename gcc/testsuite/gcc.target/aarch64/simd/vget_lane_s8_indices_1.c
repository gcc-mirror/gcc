/* { dg-do assemble } */

#include <arm_neon.h>

int8_t
test_vget_lane_s8_before (int8x8_t in)
{
  /* { dg-error "lane -1 out of range 0 - 7" "" {target *-*-*} 0 } */
  return vget_lane_s8 (in, -1);
}

int8_t
test_vget_lane_s8_beyond (int8x8_t in)
{
  /* { dg-error "lane 8 out of range 0 - 7" "" {target *-*-*} 0 } */
  return vget_lane_s8 (in, 8);
}
