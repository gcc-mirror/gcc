/* { dg-require-effective-target arm_v8_1m_mve_ok } */
/* { dg-add-options arm_v8_1m_mve } */
/* { dg-additional-options "-O2" } */

#include "arm_mve.h"

int32x4_t
foo (int32x4_t a, int32x4_t b, unsigned * carry)
{
  return vadcq_s32 (a, b, carry);
}

/* { dg-final { scan-assembler "vadc.i32"  }  } */

int32x4_t
foo1 (int32x4_t a, int32x4_t b, unsigned * carry)
{
  return vadcq (a, b, carry);
}

/* { dg-final { scan-assembler "vadc.i32"  }  } */
