/* { dg-require-effective-target arm_v8_1m_mve_ok } */
/* { dg-add-options arm_v8_1m_mve } */
/* { dg-additional-options "-O2" } */

#include "arm_mve.h"

int32x4_t
foo (int32x4_t a, int32x4_t b, int32_t c)
{
  return vmlaq_n_s32 (a, b, c);
}

/* { dg-final { scan-assembler "vmla.s32"  }  } */

int32x4_t
foo1 (int32x4_t a, int32x4_t b, int32_t c)
{
  return vmlaq (a, b, c);
}

/* { dg-final { scan-assembler "vmla.s32"  }  } */
