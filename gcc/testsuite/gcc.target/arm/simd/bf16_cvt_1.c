/* { dg-do assemble } */
/* { dg-require-effective-target arm_v8_2a_bf16_neon_ok } */
/* { dg-options "-save-temps -O2" } */
/* { dg-add-options arm_v8_2a_bf16_neon } */

#include "arm_neon.h"

float32_t test_vcvtah_f32_bf16 (bfloat16_t a)
{
  return vcvtah_f32_bf16 (a);
}

bfloat16_t test_vcvth_bf16_f32 (float32_t a)
{
  return vcvth_bf16_f32 (a);
}

float32x4_t test_vcvt_f32_bf16 (bfloat16x4_t a)
{
  return vcvt_f32_bf16 (a);
}

float32x4_t test_vcvtq_low_f32_bf16 (bfloat16x8_t a)
{
  return vcvtq_low_f32_bf16 (a);
}

float32x4_t test_vcvtq_high_f32_bf16 (bfloat16x8_t a)
{
  return vcvtq_high_f32_bf16 (a);
}

bfloat16x4_t test_vcvt_bf16_f32 (float32x4_t a)
{
  return vcvt_bf16_f32 (a);
}

bfloat16x8_t test_vcvtq_low_bf16_f32 (float32x4_t a)
{
  return vcvtq_low_bf16_f32 (a);
}

bfloat16x8_t test_vcvtq_high_bf16_f32 (bfloat16x8_t inactive, float32x4_t a)
{
  return vcvtq_high_bf16_f32 (inactive, a);
}

/* { dg-final { scan-assembler-times {vcvtb.bf16.f32\ts[0-9]+, s[0-9]+\n} 1 } } */
/* { dg-final { scan-assembler-times {vcvt.bf16.f32\td[0-9]+, q[0-9]+\n} 3 } } */
/* { dg-final { scan-assembler-times {vshl.i32\td[0-9]+, d[0-9]+, #16} 1 } } */
/* { dg-final { scan-assembler-times {vshll.u32\tq[0-9]+, d[0-9]+, #16} 3 } } */
