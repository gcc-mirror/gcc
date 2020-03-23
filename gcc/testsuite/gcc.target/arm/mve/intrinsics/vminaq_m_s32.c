/* { dg-require-effective-target arm_v8_1m_mve_ok } */
/* { dg-add-options arm_v8_1m_mve } */
/* { dg-additional-options "-O2" } */

#include "arm_mve.h"

uint32x4_t
foo (uint32x4_t a, int32x4_t b, mve_pred16_t p)
{
  return vminaq_m_s32 (a, b, p);
}

/* { dg-final { scan-assembler "vpst" } } */
/* { dg-final { scan-assembler "vminat.s32"  }  } */

uint32x4_t
foo1 (uint32x4_t a, int32x4_t b, mve_pred16_t p)
{
  return vminaq_m (a, b, p);
}

/* { dg-final { scan-assembler "vpst" } } */
