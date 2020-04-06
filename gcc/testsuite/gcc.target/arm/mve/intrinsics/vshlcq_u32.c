/* { dg-require-effective-target arm_v8_1m_mve_ok } */
/* { dg-add-options arm_v8_1m_mve } */
/* { dg-additional-options "-O2" } */

#include "arm_mve.h"

uint32x4_t
foo (uint32x4_t a, uint32_t * b)
{
  return vshlcq_u32 (a, b, 1);
}

/* { dg-final { scan-assembler "vshlc"  }  } */

uint32x4_t
foo1 (uint32x4_t a, uint32_t * b)
{
  return vshlcq (a, b, 1);
}

/* { dg-final { scan-assembler "vshlc"  }  } */
