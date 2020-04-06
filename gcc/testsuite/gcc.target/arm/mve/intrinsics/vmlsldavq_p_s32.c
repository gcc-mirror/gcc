/* { dg-require-effective-target arm_v8_1m_mve_ok } */
/* { dg-add-options arm_v8_1m_mve } */
/* { dg-additional-options "-O2" } */

#include "arm_mve.h"

int64_t
foo (int32x4_t a, int32x4_t b, mve_pred16_t p)
{
  return vmlsldavq_p_s32 (a, b, p);
}

/* { dg-final { scan-assembler "vmlsldavt.s32"  }  } */

int64_t
foo1 (int32x4_t a, int32x4_t b, mve_pred16_t p)
{
  return vmlsldavq_p (a, b, p);
}

/* { dg-final { scan-assembler "vmlsldavt.s32"  }  } */
