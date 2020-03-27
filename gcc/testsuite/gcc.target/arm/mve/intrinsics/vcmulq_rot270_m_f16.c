/* { dg-require-effective-target arm_v8_1m_mve_fp_ok } */
/* { dg-add-options arm_v8_1m_mve_fp } */
/* { dg-additional-options "-O2" } */

#include "arm_mve.h"

float16x8_t
foo (float16x8_t inactive, float16x8_t a, float16x8_t b, mve_pred16_t p)
{
  return vcmulq_rot270_m_f16 (inactive, a, b, p);
}

/* { dg-final { scan-assembler "vpst" } } */
/* { dg-final { scan-assembler "vcmult.f16"  }  } */

float16x8_t
foo1 (float16x8_t inactive, float16x8_t a, float16x8_t b, mve_pred16_t p)
{
  return vcmulq_rot270_m (inactive, a, b, p);
}

/* { dg-final { scan-assembler "vpst" } } */
/* { dg-final { scan-assembler "vcmult.f16"  }  } */
