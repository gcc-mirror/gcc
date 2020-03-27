/* { dg-require-effective-target arm_v8_1m_mve_fp_ok } */
/* { dg-add-options arm_v8_1m_mve_fp } */
/* { dg-additional-options "-O2" } */

#include "arm_mve.h"

float32x4_t
foo (float32x4_t a, float32x4_t b, float32x4_t c, mve_pred16_t p)
{
  return vcmlaq_rot180_m_f32 (a, b, c, p);
}

/* { dg-final { scan-assembler "vpst" } } */
/* { dg-final { scan-assembler "vcmlat.f32"  }  } */

float32x4_t
foo1 (float32x4_t a, float32x4_t b, float32x4_t c, mve_pred16_t p)
{
  return vcmlaq_rot180_m (a, b, c, p);
}

/* { dg-final { scan-assembler "vpst" } } */
/* { dg-final { scan-assembler "vcmlat.f32"  }  } */
