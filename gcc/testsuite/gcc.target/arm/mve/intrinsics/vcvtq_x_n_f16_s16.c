/* { dg-require-effective-target arm_v8_1m_mve_fp_ok } */
/* { dg-add-options arm_v8_1m_mve_fp } */
/* { dg-additional-options "-O2" } */

#include "arm_mve.h"

float16x8_t
foo (int16x8_t a, mve_pred16_t p)
{
  return vcvtq_x_n_f16_s16 (a, 1, p);
}

/* { dg-final { scan-assembler "vpst" } } */
/* { dg-final { scan-assembler "vcvtt.f16.s16"  }  } */

float16x8_t
foo1 (int16x8_t a, mve_pred16_t p)
{
  return vcvtq_x_n (a, 1, p);
}

/* { dg-final { scan-assembler "vpst" } } */
/* { dg-final { scan-assembler "vcvtt.f16.s16"  }  } */
