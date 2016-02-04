#include <arm_neon.h>

/* { dg-do compile } */
/* { dg-skip-if "" { *-*-* } { "-fno-fat-lto-objects" } } */
/* { dg-skip-if "" { arm*-*-* } } */

float32_t
f_vmulxs_lane_f32 (float32_t v1, float32x2_t v2)
{
  float32_t res;
  /* { dg-error "lane -1 out of range 0 - 1" "" {target *-*-*} 0 } */
  res = vmulxs_lane_f32 (v1, v2, -1);
  /* { dg-error "lane 2 out of range 0 - 1" "" {target *-*-*} 0 } */
  res = vmulxs_lane_f32 (v1, v2, 2);
  return res;
}
