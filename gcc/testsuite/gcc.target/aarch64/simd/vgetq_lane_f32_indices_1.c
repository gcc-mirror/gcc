/* { dg-do assemble } */

#include <arm_neon.h>

float32_t
test_vgetq_lane_f32_before (float32x4_t in)
{
  /* { dg-error "lane -1 out of range 0 - 3" "" {target *-*-*} 0 } */
  return vgetq_lane_f32 (in, -1);
}

float32_t
test_vgetq_lane_f32_beyond (float32x4_t in)
{
  /* { dg-error "lane 4 out of range 0 - 3" "" {target *-*-*} 0 } */
  return vgetq_lane_f32 (in, 4);
}
