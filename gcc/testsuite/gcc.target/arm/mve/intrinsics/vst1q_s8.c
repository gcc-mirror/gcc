/* { dg-require-effective-target arm_v8_1m_mve_ok } */
/* { dg-add-options arm_v8_1m_mve } */
/* { dg-additional-options "-O2" } */

#include "arm_mve.h"

void
foo (int8_t * addr, int8x16_t value)
{
  vst1q_s8 (addr, value);
}

void
foo1 (int8_t * addr, int8x16_t value)
{
  vst1q (addr, value);
}

/* { dg-final { scan-assembler-times "vstrb.8" 2 }  } */

void
foo2 (int8_t a, int8x16_t x)
{
  vst1q (&a, x);
}
