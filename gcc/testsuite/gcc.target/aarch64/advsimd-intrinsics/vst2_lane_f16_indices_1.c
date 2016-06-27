#include <arm_neon.h>

/* { dg-do compile } */
/* { dg-skip-if "" { *-*-* } { "-fno-fat-lto-objects" } } */
/* { dg-require-effective-target arm_neon_fp16_ok { target { arm*-*-* } } } */

void
f_vst2_lane_f16 (float16_t * p, float16x4x2_t v)
{
  /* { dg-error "lane 4 out of range 0 - 3" "" { target *-*-* } 0 } */
  vst2_lane_f16 (p, v, 4);
  /* { dg-error "lane -1 out of range 0 - 3" "" { target *-*-* } 0 } */
  vst2_lane_f16 (p, v, -1);
  return;
}
