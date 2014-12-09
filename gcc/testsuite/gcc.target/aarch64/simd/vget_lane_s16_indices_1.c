/* { dg-do assemble } */

#include <arm_neon.h>

int16_t
test_vget_lane_s16_before (int16x4_t in)
{
  /* { dg-error "lane -1 out of range 0 - 3" "" {target *-*-*} 0 } */
  return vget_lane_s16 (in, -1);
}

int16_t
test_vget_lane_s16_beyond (int16x4_t in)
{
  /* { dg-error "lane 4 out of range 0 - 3" "" {target *-*-*} 0 } */
  return vget_lane_s16 (in, 4);
}
