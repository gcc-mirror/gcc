/* { dg-require-effective-target arm_v8_1m_mve_fp_ok } */
/* { dg-add-options arm_v8_1m_mve_fp } */
/* { dg-additional-options "-O2" } */

#include "arm_mve.h"

void
foo (float16_t * addr, float16x8_t value)
{
  vst1q_f16 (addr, value);
}

void
foo1 (float16_t * addr, float16x8_t value)
{
  vst1q (addr, value);
}

/* { dg-final { scan-assembler-times "vstrh.16" 2 }  } */

void
foo2 (float16_t a, float16x8_t x)
{
  vst1q (&a, x);
}
