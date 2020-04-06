/* { dg-require-effective-target arm_v8_1m_mve_ok } */
/* { dg-add-options arm_v8_1m_mve } */
/* { dg-additional-options "-O2" } */

#include "arm_mve.h"

uint32x4_t
foo (int32x4_t a)
{
  return vqshluq_n_s32 (a, 7);
}

/* { dg-final { scan-assembler "vqshlu.s32"  }  } */

uint32x4_t
foo1 (int32x4_t a)
{
  return vqshluq (a, 7);
}

/* { dg-final { scan-assembler "vqshlu.s32"  }  } */
