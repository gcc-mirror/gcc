/* { dg-require-effective-target arm_v8_1m_mve_ok } */
/* { dg-add-options arm_v8_1m_mve } */
/* { dg-additional-options "-O2" } */

#include "arm_mve.h"

int32x4_t
foo (int32x4_t a)
{
  return vqshlq_n_s32 (a, 1);
}

/* { dg-final { scan-assembler "vqshl.s32"  }  } */

int32x4_t
foo1 (int32x4_t a)
{
  return vqshlq_n (a, 1);
}

/* { dg-final { scan-assembler "vqshl.s32"  }  } */
