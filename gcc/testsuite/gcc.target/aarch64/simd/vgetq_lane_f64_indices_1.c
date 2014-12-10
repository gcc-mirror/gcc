/* { dg-do assemble } */

#include <arm_neon.h>

float64_t
test_vgetq_lane_f64_before (float64x2_t in)
{
  /* { dg-error "lane -1 out of range 0 - 1" "" {target *-*-*} 0 } */
  return vgetq_lane_f64 (in, -1);
}

float64_t
test_vgetq_lane_f64_beyond (float64x2_t in)
{
  /* { dg-error "lane 2 out of range 0 - 1" "" {target *-*-*} 0 } */
  return vgetq_lane_f64 (in, 2);
}
