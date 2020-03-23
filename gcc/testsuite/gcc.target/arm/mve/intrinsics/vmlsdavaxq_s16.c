/* { dg-require-effective-target arm_v8_1m_mve_ok } */
/* { dg-add-options arm_v8_1m_mve } */
/* { dg-additional-options "-O2" } */

#include "arm_mve.h"

int32_t
foo (int32_t a, int16x8_t b, int16x8_t c)
{
  return vmlsdavaxq_s16 (a, b, c);
}

/* { dg-final { scan-assembler "vmlsdavax.s16"  }  } */

int32_t
foo1 (int32_t a, int16x8_t b, int16x8_t c)
{
  return vmlsdavaxq (a, b, c);
}

/* { dg-final { scan-assembler "vmlsdavax.s16"  }  } */
