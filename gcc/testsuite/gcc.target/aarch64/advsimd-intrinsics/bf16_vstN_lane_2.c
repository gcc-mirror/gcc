/* { dg-do assemble { target { aarch64*-*-* } } } */
/* { dg-require-effective-target arm_v8_2a_bf16_neon_ok } */
/* { dg-add-options arm_v8_2a_bf16_neon }  */
/* { dg-additional-options "-O2  --save-temps" } */

#include <arm_neon.h>

void
test_vst2_lane_bf16 (bfloat16_t *ptr, bfloat16x4x2_t b)
{
  vst2_lane_bf16 (ptr, b, 2);
}

/* { dg-final { scan-assembler-times "st2\\t{v2.h - v3.h}\\\[2\\\], \\\[x0\\\]" 1 } } */

void
test_vst2q_lane_bf16 (bfloat16_t *ptr, bfloat16x8x2_t b)
{
  vst2q_lane_bf16 (ptr, b, 2);
}

/* { dg-final { scan-assembler-times "st2\\t{v0.h - v1.h}\\\[2\\\], \\\[x0\\\]" 1 } } */

void
test_vst3_lane_bf16 (bfloat16_t *ptr, bfloat16x4x3_t b)
{
  vst3_lane_bf16 (ptr, b, 2);
}

void
test_vst3q_lane_bf16 (bfloat16_t *ptr, bfloat16x8x3_t b)
{
  vst3q_lane_bf16 (ptr, b, 2);
}

/* { dg-final { scan-assembler-times "st3\\t{v4.h - v6.h}\\\[2\\\], \\\[x0\\\]" 2 } } */

void
test_vst4_lane_bf16 (bfloat16_t *ptr, bfloat16x4x4_t b)
{
  vst4_lane_bf16 (ptr, b, 2);
}

/* { dg-final { scan-assembler-times "st4\\t{v4.h - v7.h}\\\[2\\\], \\\[x0\\\]" 1 } } */

void
test_vst4q_lane_bf16 (bfloat16_t *ptr, bfloat16x8x4_t b)
{
  vst4q_lane_bf16 (ptr, b, 2);
}

/* { dg-final { scan-assembler-times "st4\\t{v0.h - v3.h}\\\[2\\\], \\\[x0\\\]" 1 } } */
