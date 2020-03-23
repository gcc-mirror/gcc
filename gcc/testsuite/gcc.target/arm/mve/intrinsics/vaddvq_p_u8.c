/* { dg-require-effective-target arm_v8_1m_mve_ok } */
/* { dg-add-options arm_v8_1m_mve } */
/* { dg-additional-options "-O2" } */

#include "arm_mve.h"

uint32_t
foo (uint8x16_t a, mve_pred16_t p)
{
  return vaddvq_p_u8 (a, p);
}

/* { dg-final { scan-assembler "vaddvt.u8"  }  } */

uint32_t
foo1 (uint8x16_t a, mve_pred16_t p)
{
  return vaddvq_p (a, p);
}

/* { dg-final { scan-assembler "vaddvt.u8"  }  } */
