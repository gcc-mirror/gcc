#include <arm_neon.h>

/* { dg-do compile } */
/* { dg-skip-if "" { *-*-* } { "-fno-fat-lto-objects" } } */
/* { dg-skip-if "" { arm*-*-* } } */

float32_t
f_vmulxs_laneq_f32 (float32_t v1, float32x4_t v2)
{
  float32_t res;
  /* { dg-error "lane -1 out of range 0 - 3" "" {target *-*-*} 0 } */
  res = vmulxs_laneq_f32 (v1, v2, -1);
  /* { dg-error "lane 4 out of range 0 - 3" "" {target *-*-*} 0 } */
  res = vmulxs_laneq_f32 (v1, v2, 4);
  return res;
}
