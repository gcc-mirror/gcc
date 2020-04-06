/* { dg-require-effective-target arm_v8_1m_mve_fp_ok } */
/* { dg-add-options arm_v8_1m_mve_fp } */
/* { dg-additional-options "-O2" } */

#include "arm_mve.h"

float16x8_t
foo (float16_t const * base, mve_pred16_t p)
{
  return vld1q_z_f16 (base, p);
}

/* { dg-final { scan-assembler "vldrht.f16"  }  } */

float16x8_t
foo1 (float16_t const * base, mve_pred16_t p)
{
  return vld1q_z (base, p);
}

/* { dg-final { scan-assembler "vldrht.f16"  }  } */
