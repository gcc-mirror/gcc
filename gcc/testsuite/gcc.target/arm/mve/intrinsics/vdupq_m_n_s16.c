/* { dg-require-effective-target arm_v8_1m_mve_ok } */
/* { dg-add-options arm_v8_1m_mve } */
/* { dg-additional-options "-O2" } */

#include "arm_mve.h"

int16x8_t
foo (int16x8_t inactive, int16_t a, mve_pred16_t p)
{
  return vdupq_m_n_s16 (inactive, a, p);
}

/* { dg-final { scan-assembler "vpst" } } */
/* { dg-final { scan-assembler "vdupt.16"  }  } */

int16x8_t
foo1 (int16x8_t inactive, int16_t a, mve_pred16_t p)
{
  return vdupq_m (inactive, a, p);
}

/* { dg-final { scan-assembler "vpst" } } */
