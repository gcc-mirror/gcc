/* { dg-require-effective-target arm_v8_1m_mve_fp_ok } */
/* { dg-add-options arm_v8_1m_mve_fp } */
/* { dg-additional-options "-O2" } */

#include "arm_mve.h"

float32x4_t
foo (float32x4_t a, int32_t b)
{
  return vbrsrq_n_f32 (a, b);
}

/* { dg-final { scan-assembler "vbrsr.32"  }  } */

float32x4_t
foo1 (float32x4_t a, int32_t b)
{
  return vbrsrq (a, b);
}

/* { dg-final { scan-assembler "vbrsr.32"  }  } */
