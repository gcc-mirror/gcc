/* { dg-require-effective-target arm_v8_1m_mve_ok } */
/* { dg-add-options arm_v8_1m_mve } */
/* { dg-additional-options "-O2" } */

#include "arm_mve.h"

int32x4_t
foo (int32_t const * base, mve_pred16_t p)
{
  return vld1q_z_s32 (base, p);
}

/* { dg-final { scan-assembler "vldrwt.s32"  }  } */

int32x4_t
foo1 (int32_t const * base, mve_pred16_t p)
{
  return vld1q_z (base, p);
}

/* { dg-final { scan-assembler "vldrwt.s32"  }  } */
