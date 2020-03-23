/* { dg-require-effective-target arm_v8_1m_mve_ok } */
/* { dg-add-options arm_v8_1m_mve } */
/* { dg-additional-options "-O2" } */

#include "arm_mve.h"

uint8_t
foo (uint8_t a, int8x16_t b, mve_pred16_t p)
{
  return vmaxavq_p_s8 (a, b, p);
}

/* { dg-final { scan-assembler "vmaxavt.s8"  }  } */

uint8_t
foo1 (uint8_t a, int8x16_t b, mve_pred16_t p)
{
  return vmaxavq_p (a, b, p);
}

/* { dg-final { scan-assembler "vmaxavt.s8"  }  } */
