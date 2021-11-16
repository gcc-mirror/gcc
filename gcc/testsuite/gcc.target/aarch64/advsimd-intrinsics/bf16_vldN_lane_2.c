/* { dg-do assemble { target { aarch64*-*-* } } } */
/* { dg-require-effective-target arm_v8_2a_bf16_neon_ok } */
/* { dg-add-options arm_v8_2a_bf16_neon }  */
/* { dg-additional-options "-O2  --save-temps" } */

#include <arm_neon.h>

bfloat16x4x2_t
test_vld2_lane_bf16 (const bfloat16_t *ptr, bfloat16x4x2_t b)
{
  return vld2_lane_bf16 (ptr, b, 2);
}

bfloat16x8x2_t
test_vld2q_lane_bf16 (const bfloat16_t *ptr, bfloat16x8x2_t b)
{
  return vld2q_lane_bf16 (ptr, b, 2);
}

/* { dg-final { scan-assembler-times "ld2\\t{v\[0-9\]+.h - v\[0-9\]+.h}\\\[2\\\], \\\[x0\\\]" 2 } } */

bfloat16x4x3_t
test_vld3_lane_bf16 (const bfloat16_t *ptr, bfloat16x4x3_t b)
{
  return vld3_lane_bf16 (ptr, b, 2);
}

bfloat16x8x3_t
test_vld3q_lane_bf16 (const bfloat16_t *ptr, bfloat16x8x3_t b)
{
  return vld3q_lane_bf16 (ptr, b, 2);
}

/* { dg-final { scan-assembler-times "ld3\t{v\[0-9\]+.h - v\[0-9\]+.h}\\\[2\\\], \\\[x0\\\]" 2 } } */

bfloat16x4x4_t
test_vld4_lane_bf16 (const bfloat16_t *ptr, bfloat16x4x4_t b)
{
  return vld4_lane_bf16 (ptr, b, 2);
}

bfloat16x8x4_t
test_vld4q_lane_bf16 (const bfloat16_t *ptr, bfloat16x8x4_t b)
{
  return vld4q_lane_bf16 (ptr, b, 2);
}

/* { dg-final { scan-assembler-times "ld4\t{v\[0-9\]+.h - v\[0-9\]+.h}\\\[2\\\], \\\[x0\\\]" 2 } } */
