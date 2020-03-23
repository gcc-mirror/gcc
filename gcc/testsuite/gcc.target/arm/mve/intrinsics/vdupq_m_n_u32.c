/* { dg-require-effective-target arm_v8_1m_mve_ok } */
/* { dg-add-options arm_v8_1m_mve } */
/* { dg-additional-options "-O2" } */

#include "arm_mve.h"

uint32x4_t
foo (uint32x4_t inactive, uint32_t a, mve_pred16_t p)
{
  return vdupq_m_n_u32 (inactive, a, p);
}

/* { dg-final { scan-assembler "vpst" } } */
/* { dg-final { scan-assembler "vdupt.32"  }  } */

uint32x4_t
foo1 (uint32x4_t inactive, uint32_t a, mve_pred16_t p)
{
  return vdupq_m (inactive, a, p);
}

/* { dg-final { scan-assembler "vpst" } } */
