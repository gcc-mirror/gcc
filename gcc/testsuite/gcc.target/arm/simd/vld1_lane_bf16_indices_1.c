/* { dg-do assemble } */
/* { dg-require-effective-target arm_v8_2a_bf16_neon_ok } */
/* { dg-require-effective-target arm_hard_ok } */
/* { dg-add-options arm_v8_2a_bf16_neon } */
/* { dg-additional-options "-mfloat-abi=hard" } */

#include "arm_neon.h"

bfloat16x4_t
test_vld1_lane_bf16 (bfloat16_t *a, bfloat16x4_t b)
{
  bfloat16x4_t res;
  res = vld1_lane_bf16 (a, b, -1);
  res = vld1_lane_bf16 (a, b, 4);
  return res;
}

/* { dg-error "lane -1 out of range 0 - 3" "" { target *-*-* } 0 } */
/* { dg-error "lane 4 out of range 0 - 3" "" { target *-*-* } 0 } */
