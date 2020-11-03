/* { dg-do compile } */
/* { dg-skip-if "" { *-*-* } { "-fno-fat-lto-objects" } } */
/* { dg-require-effective-target arm_v8_2a_bf16_neon_ok } */
/* { dg-add-options arm_v8_2a_bf16_neon }  */

#include <arm_neon.h>

void
f_vst3q_lane_bf16 (bfloat16_t * p, bfloat16x8x3_t v)
{
  /* { dg-error "lane 8 out of range 0 - 7" "" { target *-*-* } 0 } */
  vst3q_lane_bf16 (p, v, 8);
  /* { dg-error "lane -1 out of range 0 - 7" "" { target *-*-* } 0 } */
  vst3q_lane_bf16 (p, v, -1);
  return;
}
