/* { dg-require-effective-target arm_v8_1m_mve_ok } */
/* { dg-add-options arm_v8_1m_mve } */
/* { dg-additional-options "-O2" } */

#include "arm_mve.h"

int64_t
foo (int32x4_t a, mve_pred16_t p)
{
  return vaddlvq_p_s32 (a, p);
}

/* { dg-final { scan-assembler "vaddlvt.s32"  }  } */

int64_t
foo1 (int32x4_t a, mve_pred16_t p)
{
  return vaddlvq_p (a, p);
}

/* { dg-final { scan-assembler "vaddlvt.s32"  }  } */
