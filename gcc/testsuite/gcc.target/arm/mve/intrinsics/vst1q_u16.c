/* { dg-require-effective-target arm_v8_1m_mve_ok } */
/* { dg-add-options arm_v8_1m_mve } */
/* { dg-additional-options "-O2" } */

#include "arm_mve.h"

void
foo (uint16_t * addr, uint16x8_t value)
{
  vst1q_u16 (addr, value);
}

void
foo1 (uint16_t * addr, uint16x8_t value)
{
  vst1q (addr, value);
}

/* { dg-final { scan-assembler-times "vstrh.16" 2 }  } */

void
foo2 (uint16_t a, uint16x8_t x)
{
  vst1q (&a, x);
}
