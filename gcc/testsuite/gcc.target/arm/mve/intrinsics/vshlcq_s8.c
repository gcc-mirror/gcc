/* { dg-require-effective-target arm_v8_1m_mve_ok } */
/* { dg-add-options arm_v8_1m_mve } */
/* { dg-additional-options "-O2" } */

#include "arm_mve.h"

int8x16_t
foo (int8x16_t a, uint32_t * b)
{
  return vshlcq_s8 (a, b, 1);
}

/* { dg-final { scan-assembler "vshlc"  }  } */

int8x16_t
foo1 (int8x16_t a, uint32_t * b)
{
  return vshlcq (a, b, 1);
}

/* { dg-final { scan-assembler "vshlc"  }  } */
