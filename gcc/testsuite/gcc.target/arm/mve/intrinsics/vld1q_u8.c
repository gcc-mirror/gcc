/* { dg-require-effective-target arm_v8_1m_mve_ok } */
/* { dg-add-options arm_v8_1m_mve } */
/* { dg-additional-options "-O2" } */

#include "arm_mve.h"

uint8x16_t
foo (uint8_t const * base)
{
  return vld1q_u8 (base);
}

/* { dg-final { scan-assembler "vldrb.u8"  }  } */

uint8x16_t
foo1 (uint8_t const * base)
{
  return vld1q (base);
}

/* { dg-final { scan-assembler "vldrb.u8"  }  } */
