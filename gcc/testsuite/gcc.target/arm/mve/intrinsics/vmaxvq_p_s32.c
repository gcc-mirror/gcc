/* { dg-require-effective-target arm_v8_1m_mve_ok } */
/* { dg-add-options arm_v8_1m_mve } */
/* { dg-additional-options "-O2" } */

#include "arm_mve.h"

int32_t
foo (int32_t a, int32x4_t b, mve_pred16_t p)
{
  return vmaxvq_p_s32 (a, b, p);
}

/* { dg-final { scan-assembler "vmaxvt.s32"  }  } */

int32_t
foo1 (int32_t a, int32x4_t b, mve_pred16_t p)
{
  return vmaxvq_p (a, b, p);
}

/* { dg-final { scan-assembler "vmaxvt.s32"  }  } */
