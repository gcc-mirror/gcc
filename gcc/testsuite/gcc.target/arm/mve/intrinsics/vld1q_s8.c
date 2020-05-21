/* { dg-require-effective-target arm_v8_1m_mve_ok } */
/* { dg-add-options arm_v8_1m_mve } */
/* { dg-additional-options "-O2" } */

#include "arm_mve.h"

int8x16_t
foo (int8_t const * base)
{
  return vld1q_s8 (base);
}

int8x16_t
foo1 (int8_t const * base)
{
  return vld1q (base);
}

/* { dg-final { scan-assembler-times "vldrb.8" 2 }  } */
/* { dg-final { scan-assembler-not "__ARM_undef" } } */
