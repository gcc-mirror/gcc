/* { dg-require-effective-target arm_v8_1m_mve_ok } */
/* { dg-add-options arm_v8_1m_mve } */
/* { dg-additional-options "-O2" } */

#include "arm_mve.h"

int64_t
foo (int64_t a, int32x4_t b, int32x4_t c, mve_pred16_t p)
{
  return vmlsldavaq_p_s32 (a, b, c, p);
}

/* { dg-final { scan-assembler "vmlsldavat.s32"  }  } */

int64_t
foo1 (int64_t a, int32x4_t b, int32x4_t c, mve_pred16_t p)
{
  return vmlsldavaq_p (a, b, c, p);
}

/* { dg-final { scan-assembler "vmlsldavat.s32"  }  } */
