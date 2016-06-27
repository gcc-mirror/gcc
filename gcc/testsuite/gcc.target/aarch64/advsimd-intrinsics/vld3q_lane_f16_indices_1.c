#include <arm_neon.h>

/* { dg-do compile } */
/* { dg-skip-if "" { *-*-* } { "-fno-fat-lto-objects" } } */
/* { dg-require-effective-target arm_neon_fp16_ok { target { arm*-*-* } } } */

float16x8x3_t
f_vld3q_lane_f16 (float16_t * p, float16x8x3_t v)
{
  float16x8x3_t res;
  /* { dg-error "lane 8 out of range 0 - 7" "" { target *-*-* } 0 } */
  res = vld3q_lane_f16 (p, v, 8);
  /* { dg-error "lane -1 out of range 0 - 7" "" { target *-*-* } 0 } */
  res = vld3q_lane_f16 (p, v, -1);
  return res;
}
