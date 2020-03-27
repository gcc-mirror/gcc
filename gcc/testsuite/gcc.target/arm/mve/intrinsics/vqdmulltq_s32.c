/* { dg-require-effective-target arm_v8_1m_mve_ok } */
/* { dg-add-options arm_v8_1m_mve } */
/* { dg-additional-options "-O2" } */

#include "arm_mve.h"

int64x2_t
foo (int32x4_t a, int32x4_t b)
{
  return vqdmulltq_s32 (a, b);
}

/* { dg-final { scan-assembler "vqdmullt.s32"  }  } */

int64x2_t
foo1 (int32x4_t a, int32x4_t b)
{
  return vqdmulltq (a, b);
}

/* { dg-final { scan-assembler "vqdmullt.s32"  }  } */
