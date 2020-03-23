/* { dg-require-effective-target arm_v8_1m_mve_fp_ok } */
/* { dg-add-options arm_v8_1m_mve_fp } */
/* { dg-additional-options "-O2" } */

#include "arm_mve.h"

mve_pred16_t
foo (float16x8_t a, float16x8_t b)
{
  return vcmpleq_f16 (a, b);
}

/* { dg-final { scan-assembler "vcmp.f16"  }  } */

mve_pred16_t
foo1 (float16x8_t a, float16x8_t b)
{
  return vcmpleq (a, b);
}

/* { dg-final { scan-assembler "vcmp.f16"  }  } */
