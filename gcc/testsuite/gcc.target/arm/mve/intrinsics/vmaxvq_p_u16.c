/* { dg-require-effective-target arm_v8_1m_mve_ok } */
/* { dg-add-options arm_v8_1m_mve } */
/* { dg-additional-options "-O2" } */

#include "arm_mve.h"

uint16_t
foo (uint16_t a, uint16x8_t b, mve_pred16_t p)
{
  return vmaxvq_p_u16 (a, b, p);
}

/* { dg-final { scan-assembler "vmaxvt.u16"  }  } */

uint16_t
foo1 (uint16_t a, uint16x8_t b, mve_pred16_t p)
{
  return vmaxvq_p (a, b, p);
}

/* { dg-final { scan-assembler "vmaxvt.u16"  }  } */
