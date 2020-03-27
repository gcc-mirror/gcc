/* { dg-require-effective-target arm_v8_1m_mve_fp_ok } */
/* { dg-add-options arm_v8_1m_mve_fp } */
/* { dg-additional-options "-O2" } */

#include "arm_mve.h"

float16x8_t
foo (uint16x8_t a)
{
  return vcvtq_n_f16_u16 (a, 1);
}

/* { dg-final { scan-assembler "vcvt.f16.u16"  }  } */

float16x8_t
foo1 (uint16x8_t a)
{
  return vcvtq_n (a, 1);
}

/* { dg-final { scan-assembler "vcvt.f16.u16"  }  } */
