/* { dg-require-effective-target arm_v8_1m_mve_fp_ok } */
/* { dg-add-options arm_v8_1m_mve_fp } */
/* { dg-additional-options "-O2" } */

#include "arm_mve.h"

float32x4_t
foo (float16x8_t a)
{
  return vcvtbq_f32_f16 (a);
}

/* { dg-final { scan-assembler "vcvtb.f32.f16"  }  } */
