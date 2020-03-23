/* { dg-require-effective-target arm_v8_1m_mve_ok } */
/* { dg-add-options arm_v8_1m_mve } */
/* { dg-additional-options "-O2" } */

#include "arm_mve.h"

int8x16_t
foo (int8x16_t inactive, int8_t a, mve_pred16_t p)
{
  return vdupq_m_n_s8 (inactive, a, p);
}

/* { dg-final { scan-assembler "vpst" } } */
/* { dg-final { scan-assembler "vdupt.8"  }  } */

int8x16_t
foo1 (int8x16_t inactive, int8_t a, mve_pred16_t p)
{
  return vdupq_m (inactive, a, p);
}

/* { dg-final { scan-assembler "vpst" } } */
