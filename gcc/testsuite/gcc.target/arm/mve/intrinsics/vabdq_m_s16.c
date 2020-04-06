/* { dg-require-effective-target arm_v8_1m_mve_ok } */
/* { dg-add-options arm_v8_1m_mve } */
/* { dg-additional-options "-O2" } */

#include "arm_mve.h"

int16x8_t
foo (int16x8_t inactive, int16x8_t a, int16x8_t b, mve_pred16_t p)
{
  return vabdq_m_s16 (inactive, a, b, p);
}

/* { dg-final { scan-assembler "vpst" } } */
/* { dg-final { scan-assembler "vabdt.s16"  }  } */

int16x8_t
foo1 (int16x8_t inactive, int16x8_t a, int16x8_t b, mve_pred16_t p)
{
  return vabdq_m (inactive, a, b, p);
}

/* { dg-final { scan-assembler "vpst" } } */
/* { dg-final { scan-assembler "vabdt.s16"  }  } */
