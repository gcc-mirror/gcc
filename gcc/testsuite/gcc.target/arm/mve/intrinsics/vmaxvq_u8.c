/* { dg-require-effective-target arm_v8_1m_mve_ok } */
/* { dg-add-options arm_v8_1m_mve } */
/* { dg-additional-options "-O2" } */

#include "arm_mve.h"

uint8_t
foo (uint8_t a, uint8x16_t b)
{
  return vmaxvq_u8 (a, b);
}

/* { dg-final { scan-assembler "vmaxv.u8"  }  } */

uint8_t
foo1 (uint8_t a, uint8x16_t b)
{
  return vmaxvq (a, b);
}

/* { dg-final { scan-assembler "vmaxv.u8"  }  } */
