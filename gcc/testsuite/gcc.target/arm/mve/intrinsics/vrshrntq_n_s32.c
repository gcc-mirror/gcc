/* { dg-require-effective-target arm_v8_1m_mve_ok } */
/* { dg-add-options arm_v8_1m_mve } */
/* { dg-additional-options "-O2" } */

#include "arm_mve.h"

int16x8_t
foo (int16x8_t a, int32x4_t b)
{
  return vrshrntq_n_s32 (a, b, 1);
}

/* { dg-final { scan-assembler "vrshrnt.i32"  }  } */

int16x8_t
foo1 (int16x8_t a, int32x4_t b)
{
  return vrshrntq (a, b, 1);
}

/* { dg-final { scan-assembler "vrshrnt.i32"  }  } */
