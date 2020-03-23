/* { dg-require-effective-target arm_v8_1m_mve_ok } */
/* { dg-add-options arm_v8_1m_mve } */
/* { dg-additional-options "-O2" } */

#include "arm_mve.h"

uint32_t
foo (uint32_t a, uint8x16_t b)
{
  return vaddvaq_u8 (a, b);
}

/* { dg-final { scan-assembler "vaddva.u8"  }  } */

uint32_t
foo1 (uint32_t a, uint8x16_t b)
{
  return vaddvaq (a, b);
}

/* { dg-final { scan-assembler "vaddva.u8"  }  } */
