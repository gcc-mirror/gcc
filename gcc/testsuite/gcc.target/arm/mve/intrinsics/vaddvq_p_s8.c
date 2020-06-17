/* { dg-require-effective-target arm_v8_1m_mve_ok } */
/* { dg-add-options arm_v8_1m_mve } */
/* { dg-additional-options "-O2" } */

#include "arm_mve.h"

int32_t
foo (int8x16_t a, mve_pred16_t p)
{
  return vaddvq_p_s8 (a, p);
}

/* { dg-final { scan-assembler "vaddvt.s8"  }  } */

int32_t
foo1 (int8x16_t a, mve_pred16_t p)
{
  return vaddvq_p (a, p);
}

/* { dg-final { scan-assembler "vaddvt.s8"  }  } */
