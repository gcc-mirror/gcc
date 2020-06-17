/* { dg-require-effective-target arm_v8_1m_mve_ok } */
/* { dg-add-options arm_v8_1m_mve } */
/* { dg-additional-options "-O2" } */

#include "arm_mve.h"

int32x4_t
foo (int16x8_t a)
{
  return vshllbq_n_s16 (a, 1);
}

/* { dg-final { scan-assembler "vshllb.s16"  }  } */

int32x4_t
foo1 (int16x8_t a)
{
  return vshllbq (a, 1);
}

/* { dg-final { scan-assembler "vshllb.s16"  }  } */
