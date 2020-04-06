/* { dg-require-effective-target arm_v8_1m_mve_ok } */
/* { dg-add-options arm_v8_1m_mve } */
/* { dg-additional-options "-O2" } */

#include "arm_mve.h"

uint8x16_t
foo (uint8x16_t inactive, int8x16_t a, mve_pred16_t p)
{
  return vqshluq_m_n_s8 (inactive, a, 7, p);
}

/* { dg-final { scan-assembler "vpst" } } */
/* { dg-final { scan-assembler "vqshlut.s8"  }  } */

uint8x16_t
foo1 (uint8x16_t inactive, int8x16_t a, mve_pred16_t p)
{
  return vqshluq_m (inactive, a, 7, p);
}

/* { dg-final { scan-assembler "vpst" } } */
