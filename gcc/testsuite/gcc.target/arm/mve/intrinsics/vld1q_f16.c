/* { dg-require-effective-target arm_v8_1m_mve_fp_ok } */
/* { dg-add-options arm_v8_1m_mve_fp } */
/* { dg-additional-options "-O2" } */

#include "arm_mve.h"

float16x8_t
foo (float16_t const * base)
{
  return vld1q_f16 (base);
}

/* { dg-final { scan-assembler "vldrh.f16"  }  } */

float16x8_t
foo1 (float16_t const * base)
{
  return vld1q (base);
}

/* { dg-final { scan-assembler "vldrh.f16"  }  } */
