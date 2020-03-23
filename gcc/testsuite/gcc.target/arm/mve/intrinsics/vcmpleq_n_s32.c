/* { dg-require-effective-target arm_v8_1m_mve_ok } */
/* { dg-add-options arm_v8_1m_mve } */
/* { dg-additional-options "-O2" } */

#include "arm_mve.h"

mve_pred16_t
foo (int32x4_t a, int32_t b)
{
  return vcmpleq_n_s32 (a, b);
}

/* { dg-final { scan-assembler "vcmp.s32"  }  } */

mve_pred16_t
foo1 (int32x4_t a, int32_t b)
{
  return vcmpleq (a, b);
}

/* { dg-final { scan-assembler "vcmp.s32"  }  } */
