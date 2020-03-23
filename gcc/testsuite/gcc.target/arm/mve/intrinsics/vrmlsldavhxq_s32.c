/* { dg-require-effective-target arm_v8_1m_mve_ok } */
/* { dg-add-options arm_v8_1m_mve } */
/* { dg-additional-options "-O2" } */

#include "arm_mve.h"

int64_t
foo (int32x4_t a, int32x4_t b)
{
  return vrmlsldavhxq_s32 (a, b);
}

/* { dg-final { scan-assembler "vrmlsldavhx.s32"  }  } */

int64_t
foo1 (int32x4_t a, int32x4_t b)
{
  return vrmlsldavhxq (a, b);
}

/* { dg-final { scan-assembler "vrmlsldavhx.s32"  }  } */
