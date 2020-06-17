/* { dg-require-effective-target arm_v8_1m_mve_fp_ok } */
/* { dg-add-options arm_v8_1m_mve_fp } */
/* { dg-additional-options "-O2" } */

#include "arm_mve.h"

float16x8_t
foo (float16x8_t a, float16_t b)
{
  return vmulq_n_f16 (a, b);
}

/* { dg-final { scan-assembler "vmul.f16"  }  } */

float16x8_t
foo1 (float16x8_t a, float16_t b)
{
  return vmulq (a, b);
}

/* { dg-final { scan-assembler "vmul.f16"  }  } */
