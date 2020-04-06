/* { dg-require-effective-target arm_v8_1m_mve_ok } */
/* { dg-add-options arm_v8_1m_mve } */
/* { dg-additional-options "-O2" } */

#include "arm_mve.h"

uint32_t
foo (uint32_t a, uint8x16_t b, uint8x16_t c)
{
  return vabavq_u8 (a, b, c);
}

/* { dg-final { scan-assembler "vabav.u8"  }  } */

uint32_t
foo1 (uint32_t a, uint8x16_t b, uint8x16_t c)
{
  return vabavq (a, b, c);
}

/* { dg-final { scan-assembler "vabav.u8"  }  } */
