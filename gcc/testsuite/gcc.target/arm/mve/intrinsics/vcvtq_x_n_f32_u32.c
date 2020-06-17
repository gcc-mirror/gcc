/* { dg-require-effective-target arm_v8_1m_mve_fp_ok } */
/* { dg-add-options arm_v8_1m_mve_fp } */
/* { dg-additional-options "-O2" } */

#include "arm_mve.h"

float32x4_t
foo (uint32x4_t a, mve_pred16_t p)
{
  return vcvtq_x_n_f32_u32 (a, 16, p);
}

/* { dg-final { scan-assembler "vpst" } } */
/* { dg-final { scan-assembler "vcvtt.f32.u32"  }  } */

float32x4_t
foo1 (uint32x4_t a, mve_pred16_t p)
{
  return vcvtq_x_n (a, 16, p);
}

/* { dg-final { scan-assembler "vpst" } } */
/* { dg-final { scan-assembler "vcvtt.f32.u32"  }  } */
