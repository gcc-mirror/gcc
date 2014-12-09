/* { dg-do assemble } */

#include <arm_neon.h>

int32_t
test_vget_lane_s32_before (int32x2_t in)
{
  /* { dg-error "lane -1 out of range 0 - 1" "" {target *-*-*} 0 } */
  return vget_lane_s32 (in, -1);
}

int32_t
test_vget_lane_s32_beyond (int32x2_t in)
{
  /* { dg-error "lane 2 out of range 0 - 1" "" {target *-*-*} 0 } */
  return vget_lane_s32 (in, 2);
}
