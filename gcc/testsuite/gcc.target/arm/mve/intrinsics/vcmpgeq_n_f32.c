/* { dg-require-effective-target arm_v8_1m_mve_fp_ok } */
/* { dg-add-options arm_v8_1m_mve_fp } */
/* { dg-additional-options "-O2" } */

#include "arm_mve.h"

mve_pred16_t
foo (float32x4_t a, float32_t b)
{
  return vcmpgeq_n_f32 (a, b);
}

/* { dg-final { scan-assembler "vcmp.f32"  }  } */

mve_pred16_t
foo1 (float32x4_t a, float32_t b)
{
  return vcmpgeq (a, b);
}

/* { dg-final { scan-assembler "vcmp.f32"  }  } */
