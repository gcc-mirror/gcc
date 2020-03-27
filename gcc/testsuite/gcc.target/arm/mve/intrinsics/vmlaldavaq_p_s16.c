/* { dg-require-effective-target arm_v8_1m_mve_ok } */
/* { dg-add-options arm_v8_1m_mve } */
/* { dg-additional-options "-O2" } */

#include "arm_mve.h"

int64_t
foo (int64_t a, int16x8_t b, int16x8_t c, mve_pred16_t p)
{
  return vmlaldavaq_p_s16 (a, b, c, p);
}

/* { dg-final { scan-assembler "vmlaldavat.s16"  }  } */

int64_t
foo1 (int64_t a, int16x8_t b, int16x8_t c, mve_pred16_t p)
{
  return vmlaldavaq_p (a, b, c, p);
}

/* { dg-final { scan-assembler "vmlaldavat.s16"  }  } */
