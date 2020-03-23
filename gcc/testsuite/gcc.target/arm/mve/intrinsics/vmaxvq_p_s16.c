/* { dg-require-effective-target arm_v8_1m_mve_ok } */
/* { dg-add-options arm_v8_1m_mve } */
/* { dg-additional-options "-O2" } */

#include "arm_mve.h"

int16_t
foo (int16_t a, int16x8_t b, mve_pred16_t p)
{
  return vmaxvq_p_s16 (a, b, p);
}

/* { dg-final { scan-assembler "vmaxvt.s16"  }  } */

int16_t
foo1 (int16_t a, int16x8_t b, mve_pred16_t p)
{
  return vmaxvq_p (a, b, p);
}

/* { dg-final { scan-assembler "vmaxvt.s16"  }  } */
