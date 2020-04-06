/* { dg-require-effective-target arm_v8_1m_mve_ok } */
/* { dg-add-options arm_v8_1m_mve } */
/* { dg-additional-options "-O2" } */

#include "arm_mve.h"

int32x4_t
foo (int32x4_t inactive, int16x8_t a, mve_pred16_t p)
{
  return vmovltq_m_s16 (inactive, a, p);
}

/* { dg-final { scan-assembler "vpst" } } */
/* { dg-final { scan-assembler "vmovltt.s16"  }  } */

int32x4_t
foo1 (int32x4_t inactive, int16x8_t a, mve_pred16_t p)
{
  return vmovltq_m (inactive, a, p);
}

/* { dg-final { scan-assembler "vpst" } } */
