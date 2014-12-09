/* { dg-do assemble } */

#include <arm_neon.h>

float64_t
test_vget_lane_f64_before (float64x1_t in)
{
  /* { dg-error "lane -1 out of range 0 - 0" "" {target *-*-*} 0 } */
  return vget_lane_f64 (in, -1);
}

float64_t
test_vget_lane_f64_beyond (float64x1_t in)
{
  /* { dg-error "lane 1 out of range 0 - 0" "" {target *-*-*} 0 } */
  return vget_lane_f64 (in, 1);
}
