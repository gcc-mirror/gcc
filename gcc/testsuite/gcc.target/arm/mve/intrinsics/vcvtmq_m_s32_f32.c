/* { dg-require-effective-target arm_v8_1m_mve_fp_ok } */
/* { dg-add-options arm_v8_1m_mve_fp } */
/* { dg-additional-options "-O2" } */

#include "arm_mve.h"

int32x4_t
foo (int32x4_t inactive, float32x4_t a, mve_pred16_t p)
{
  return vcvtmq_m_s32_f32 (inactive, a, p);
}

/* { dg-final { scan-assembler "vpst" } } */
/* { dg-final { scan-assembler "vcvtmt.s32.f32"  }  } */

int32x4_t
foo1 (int32x4_t inactive, float32x4_t a, mve_pred16_t p)
{
  return vcvtmq_m (inactive, a, p);
}

/* { dg-final { scan-assembler "vpst" } } */
