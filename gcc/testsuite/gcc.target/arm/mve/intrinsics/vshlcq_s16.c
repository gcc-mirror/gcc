/* { dg-require-effective-target arm_v8_1m_mve_ok } */
/* { dg-add-options arm_v8_1m_mve } */
/* { dg-additional-options "-O2" } */

#include "arm_mve.h"

int16x8_t
foo (int16x8_t a, uint32_t * b)
{
  return vshlcq_s16 (a, b, 1);
}

/* { dg-final { scan-assembler "vshlc"  }  } */

int16x8_t
foo1 (int16x8_t a, uint32_t * b)
{
  return vshlcq (a, b, 1);
}

/* { dg-final { scan-assembler "vshlc"  }  } */
