/* { dg-require-effective-target arm_v8_1m_mve_ok } */
/* { dg-add-options arm_v8_1m_mve } */
/* { dg-additional-options "-O2" } */

#include "arm_mve.h"

int8x16_t
foo (int8x16_t a, int16x8_t b, mve_pred16_t p)
{
  return vqmovnbq_m_s16 (a, b, p);
}

/* { dg-final { scan-assembler "vpst" } } */
/* { dg-final { scan-assembler "vqmovnbt.s16"  }  } */

int8x16_t
foo1 (int8x16_t a, int16x8_t b, mve_pred16_t p)
{
  return vqmovnbq_m (a, b, p);
}

/* { dg-final { scan-assembler "vpst" } } */
