/* { dg-require-effective-target arm_v8_1m_mve_ok } */
/* { dg-add-options arm_v8_1m_mve } */
/* { dg-additional-options "-O2" } */

#include "arm_mve.h"

mve_pred16_t
foo (uint32x4_t a, uint32_t b, mve_pred16_t p)
{
  return vcmphiq_m_n_u32 (a, b, p);
}

/* { dg-final { scan-assembler "vpst" } } */
/* { dg-final { scan-assembler "vcmpt.u32"  }  } */

mve_pred16_t
foo1 (uint32x4_t a, uint32_t b, mve_pred16_t p)
{
  return vcmphiq_m (a, b, p);
}

/* { dg-final { scan-assembler "vpst" } } */
