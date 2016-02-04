#include <arm_neon.h>

/* { dg-do compile } */
/* { dg-skip-if "" { *-*-* } { "-fno-fat-lto-objects" } } */
/* { dg-skip-if "" { arm*-*-* } } */

float64x2_t
f_vmulxq_lane_f64 (float64x2_t v1, float64x1_t v2)
{
  float64x2_t res;
  /* { dg-error "lane -1 out of range 0 - 0" "" {target *-*-*} 0 } */
  res = vmulxq_lane_f64 (v1, v2, -1);
  /* { dg-error "lane 1 out of range 0 - 0" "" {target *-*-*} 0 } */
  res = vmulxq_lane_f64 (v1, v2, 1);
  return res;
}
