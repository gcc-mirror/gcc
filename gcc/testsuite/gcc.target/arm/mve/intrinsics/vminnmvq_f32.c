/* { dg-require-effective-target arm_v8_1m_mve_fp_ok } */
/* { dg-add-options arm_v8_1m_mve_fp } */
/* { dg-additional-options "-O2" } */

#include "arm_mve.h"

float32_t
foo (float32_t a, float32x4_t b)
{
  return vminnmvq_f32 (a, b);
}


float32_t
foo1 (float32_t a, float32x4_t b)
{
  return vminnmvq (a, b);
}


float32_t
foo2 (float16_t a, float32x4_t b)
{
  return vminnmvq (a, b);
}

/* { dg-final { scan-assembler-not "__ARM_undef" } } */
/* { dg-final { scan-assembler-times "vminnmv.f32" 3 } } */
