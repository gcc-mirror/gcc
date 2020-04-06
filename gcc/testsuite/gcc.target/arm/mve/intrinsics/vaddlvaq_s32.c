/* { dg-require-effective-target arm_v8_1m_mve_ok } */
/* { dg-add-options arm_v8_1m_mve } */
/* { dg-additional-options "-O2" } */

#include "arm_mve.h"

int64_t
foo (int64_t a, int32x4_t b)
{
  return vaddlvaq_s32 (a, b);
}

/* { dg-final { scan-assembler "vaddlva.s32"  }  } */

int64_t
foo1 (int64_t a, int32x4_t b)
{
  return vaddlvaq (a, b);
}

/* { dg-final { scan-assembler "vaddlva.s32"  }  } */
