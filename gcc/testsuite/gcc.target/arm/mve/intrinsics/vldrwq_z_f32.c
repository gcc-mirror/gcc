/* { dg-require-effective-target arm_v8_1m_mve_fp_ok } */
/* { dg-add-options arm_v8_1m_mve_fp } */
/* { dg-additional-options "-O2" } */

#include "arm_mve.h"

float32x4_t
foo (float32_t const * base, mve_pred16_t p)
{
  return vldrwq_z_f32 (base, p);
}

/* { dg-final { scan-assembler "vldrwt.f32"  }  } */
