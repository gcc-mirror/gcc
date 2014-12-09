/* { dg-do assemble } */

#include <arm_neon.h>

float32_t
test_vget_lane_f32_before (float32x2_t in)
{
  /* { dg-error "lane -1 out of range 0 - 1" "" {target *-*-*} 0 } */
  return vget_lane_f32 (in, -1);
}

float32_t
test_vget_lane_f32_beyond (float32x2_t in)
{
  /* { dg-error "lane 2 out of range 0 - 1" "" {target *-*-*} 0 } */
  return vget_lane_f32 (in, 2);
}
