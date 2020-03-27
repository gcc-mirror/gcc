/* { dg-require-effective-target arm_v8_1m_mve_fp_ok } */
/* { dg-add-options arm_v8_1m_mve_fp } */
/* { dg-additional-options "-O2" } */

#include "arm_mve.h"

float32x4_t
foo (float32x4_t inactive, float32x4_t a, int32_t b, mve_pred16_t p)
{
  return vbrsrq_m_n_f32 (inactive, a, b, p);
}

/* { dg-final { scan-assembler "vpst" } } */
/* { dg-final { scan-assembler "vbrsrt.32"  }  } */

float32x4_t
foo1 (float32x4_t inactive, float32x4_t a, int32_t b, mve_pred16_t p)
{
  return vbrsrq_m (inactive, a, b, p);
}

/* { dg-final { scan-assembler "vpst" } } */
/* { dg-final { scan-assembler "vbrsrt.32"  }  } */
