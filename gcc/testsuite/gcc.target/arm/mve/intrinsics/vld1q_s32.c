/* { dg-require-effective-target arm_v8_1m_mve_ok } */
/* { dg-add-options arm_v8_1m_mve } */
/* { dg-additional-options "-O2" } */

#include "arm_mve.h"

int32x4_t
foo (int32_t const * base)
{
  return vld1q_s32 (base);
}

/* { dg-final { scan-assembler "vldrw.s32"  }  } */

int32x4_t
foo1 (int32_t const * base)
{
  return vld1q (base);
}

/* { dg-final { scan-assembler "vldrw.s32"  }  } */
