/* { dg-do assemble } */
/* { dg-require-effective-target arm_v8_2a_bf16_neon_ok } */
/* { dg-require-effective-target arm_hard_ok } */
/* { dg-add-options arm_v8_2a_bf16_neon } */
/* { dg-additional-options "-O3 --save-temps -mfloat-abi=hard" } */

#include "arm_neon.h"

bfloat16x4_t
test_vld1_lane_bf16 (bfloat16_t *a, bfloat16x4_t b)
{
  return vld1_lane_bf16 (a, b, 1);
}

bfloat16x8_t
test_vld1q_lane_bf16 (bfloat16_t *a, bfloat16x8_t b)
{
  return vld1q_lane_bf16 (a, b, 2);
}

/* { dg-final { scan-assembler "vld1.16\t{d0\\\[1\\\]}, \\\[r0\\\]" } } */
/* { dg-final { scan-assembler "vld1.16\t{d0\\\[2\\\]}, \\\[r0\\\]" } } */
