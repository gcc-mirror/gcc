/* { dg-do assemble } */
/* { dg-require-effective-target arm_v8_2a_bf16_neon_ok } */
/* { dg-require-effective-target arm_hard_ok } */
/* { dg-add-options arm_v8_2a_bf16_neon } */
/* { dg-additional-options "-mfloat-abi=hard" } */

#include "arm_neon.h"

void
test_vstq1_lane_bf16 (bfloat16_t *a, bfloat16x8_t b)
{
  vst1q_lane_bf16 (a, b, -1);
  vst1q_lane_bf16 (a, b, 8);
}

/* { dg-error "lane -1 out of range 0 - 7" "" { target *-*-* } 0 } */
/* { dg-error "lane 8 out of range 0 - 7" "" { target *-*-* } 0 } */
