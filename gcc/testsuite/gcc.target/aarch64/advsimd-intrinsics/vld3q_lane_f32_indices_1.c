#include <arm_neon.h>

/* { dg-do compile } */
/* { dg-skip-if "" { *-*-* } { "-fno-fat-lto-objects" } } */

float32x4x3_t
f_vld3q_lane_f32 (float32_t * p, float32x4x3_t v)
{
  float32x4x3_t res;
  /* { dg-error "lane 4 out of range 0 - 3" "" { target *-*-* } 0 } */
  res = vld3q_lane_f32 (p, v, 4);
  /* { dg-error "lane -1 out of range 0 - 3" "" { target *-*-* } 0 } */
  res = vld3q_lane_f32 (p, v, -1);
  return res;
}
