/* { dg-require-effective-target arm_v8_1m_mve_fp_ok } */
/* { dg-add-options arm_v8_1m_mve_fp } */
/* { dg-additional-options "-O2" } */

#include "arm_mve.h"

uint16x8_t
foo (uint16x8_t inactive, float16x8_t a, mve_pred16_t p)
{
  return vcvtmq_m_u16_f16 (inactive, a, p);
}

/* { dg-final { scan-assembler "vpst" } } */
/* { dg-final { scan-assembler "vcvtmt.u16.f16"  }  } */

uint16x8_t
foo1 (uint16x8_t inactive, float16x8_t a, mve_pred16_t p)
{
  return vcvtmq_m (inactive, a, p);
}

/* { dg-final { scan-assembler "vpst" } } */
