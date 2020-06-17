/* { dg-require-effective-target arm_v8_1m_mve_ok } */
/* { dg-add-options arm_v8_1m_mve } */
/* { dg-additional-options "-O2" } */

#include "arm_mve.h"

int32x4_t
foo (int32x4_t inactive, int32x4_t a, int32x4_t b, unsigned * carry, mve_pred16_t p)
{
  return vadcq_m_s32 (inactive, a, b, carry, p);
}

/* { dg-final { scan-assembler "vpst" } } */
/* { dg-final { scan-assembler "vadct.i32"  }  } */

int32x4_t
foo1 (int32x4_t inactive, int32x4_t a, int32x4_t b, unsigned * carry, mve_pred16_t p)
{
  return vadcq_m (inactive, a, b, carry, p);
}

/* { dg-final { scan-assembler "vpst" } } */
/* { dg-final { scan-assembler "vadct.i32"  }  } */
