/* { dg-do assemble } */

#include <arm_neon.h>

int32_t
test_vgetq_lane_s32_before (int32x4_t in)
{
  /* { dg-error "lane -1 out of range 0 - 3" "" {target *-*-*} 0 } */
  return vgetq_lane_s32 (in, -1);
}

int32_t
test_vgetq_lane_s32_beyond (int32x4_t in)
{
  /* { dg-error "lane 4 out of range 0 - 3" "" {target *-*-*} 0 } */
  return vgetq_lane_s32 (in, 4);
}
