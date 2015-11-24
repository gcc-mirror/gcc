#include <arm_neon.h>

/* { dg-do compile } */
/* { dg-skip-if "" { *-*-* } { "-fno-fat-lto-objects" } } */
/* { dg-skip-if "" { arm*-*-* } } */

float64x1_t
f_vmulx_laneq_f64 (float64x1_t v1, float64x2_t v2)
{
  float64x1_t res;
  /* { dg-error "lane -1 out of range 0 - 1" "" {target *-*-*} 0 } */
  res = vmulx_laneq_f64 (v1, v2, -1);
  /* { dg-error "lane 2 out of range 0 - 1" "" {target *-*-*} 0 } */
  res = vmulx_laneq_f64 (v1, v2, 2);
  return res;
}
