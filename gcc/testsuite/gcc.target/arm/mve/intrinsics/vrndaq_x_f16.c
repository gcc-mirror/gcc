/* { dg-require-effective-target arm_v8_1m_mve_fp_ok } */
/* { dg-add-options arm_v8_1m_mve_fp } */
/* { dg-additional-options "-O2" } */

#include "arm_mve.h"

float16x8_t
foo (float16x8_t a, mve_pred16_t p)
{
  return vrndaq_x_f16 (a, p);
}

/* { dg-final { scan-assembler "vpst" } } */
/* { dg-final { scan-assembler "vrintat.f16"  }  } */

float16x8_t
foo1 (float16x8_t a, mve_pred16_t p)
{
  return vrndaq_x (a, p);
}

/* { dg-final { scan-assembler "vpst" } } */
