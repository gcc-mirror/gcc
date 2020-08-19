/* { dg-require-effective-target arm_v8_1m_mve_ok } */
/* { dg-add-options arm_v8_1m_mve } */
/* { dg-additional-options "-O2" } */

#include "arm_mve.h"

void
foo (uint8_t * addr, uint8x16_t value)
{
  vst1q_u8 (addr, value);
}

void
foo1 (uint8_t * addr, uint8x16_t value)
{
  vst1q (addr, value);
}

/* { dg-final { scan-assembler-times "vstrb.8" 2 }  } */

void
foo2 (uint8_t a, uint8x16_t x)
{
  vst1q (&a, x);
}
