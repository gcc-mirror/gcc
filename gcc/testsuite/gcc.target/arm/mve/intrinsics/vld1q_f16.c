/* { dg-require-effective-target arm_v8_1m_mve_fp_ok } */
/* { dg-add-options arm_v8_1m_mve_fp } */
/* { dg-additional-options "-O2" } */

#include "arm_mve.h"

float16x8_t
foo (float16_t const * base)
{
  return vld1q_f16 (base);
}

float16x8_t
foo1 (float16_t const * base)
{
  return vld1q (base);
}

/* { dg-final { scan-assembler-times "vldrh.16" 2 }  } */
/* { dg-final { scan-assembler-not "__ARM_undef" } } */
