/* { dg-require-effective-target arm_v8_1m_mve_ok } */
/* { dg-add-options arm_v8_1m_mve } */
/* { dg-additional-options "-O2" } */

#include "arm_mve.h"

mve_pred16_t
foo (int8x16_t a, int8x16_t b, mve_pred16_t p)
{
  return vcmpleq_m_s8 (a, b, p);
}

/* { dg-final { scan-assembler "vpst" } } */
/* { dg-final { scan-assembler "vcmpt.s8"  }  } */

mve_pred16_t
foo1 (int8x16_t a, int8x16_t b, mve_pred16_t p)
{
  return vcmpleq_m (a, b, p);
}

/* { dg-final { scan-assembler "vpst" } } */
