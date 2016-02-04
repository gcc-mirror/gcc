#include <arm_neon.h>

/* { dg-do compile } */
/* { dg-skip-if "" { *-*-* } { "-fno-fat-lto-objects" } } */
/* { dg-skip-if "" { arm*-*-* } } */

float64x1_t
f_vmulx_lane_f64 (float64x1_t v1, float64x1_t v2)
{
  float64x1_t res;
  /* { dg-error "lane -1 out of range 0 - 0" "" { target *-*-* } 0 } */
  res = vmulx_lane_f64 (v1, v2, -1);
  /* { dg-error "lane 1 out of range 0 - 0" "" { target *-*-* } 0 } */
  res = vmulx_lane_f64 (v1, v2, 1);
  return res;
}
