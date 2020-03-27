/* { dg-require-effective-target arm_v8_1m_mve_ok } */
/* { dg-add-options arm_v8_1m_mve } */
/* { dg-additional-options "-O2" } */

#include "arm_mve.h"

mve_pred16_t
foo (uint8x16_t a, uint8_t b, mve_pred16_t p)
{
  return vcmpneq_m_n_u8 (a, b, p);
}

/* { dg-final { scan-assembler "vpst" } } */
/* { dg-final { scan-assembler "vcmpt.i8"  }  } */

mve_pred16_t
foo1 (uint8x16_t a, uint8_t b, mve_pred16_t p)
{
  return vcmpneq_m (a, b, p);
}

/* { dg-final { scan-assembler "vpst" } } */
