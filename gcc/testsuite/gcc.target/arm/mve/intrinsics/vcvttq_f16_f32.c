/* { dg-require-effective-target arm_v8_1m_mve_fp_ok } */
/* { dg-add-options arm_v8_1m_mve_fp } */
/* { dg-additional-options "-O2" } */

#include "arm_mve.h"

float16x8_t
foo (float16x8_t a, float32x4_t b)
{
  return vcvttq_f16_f32 (a, b);
}

/* { dg-final { scan-assembler "vcvtt.f16.f32"  }  } */
