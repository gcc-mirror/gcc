/* { dg-require-effective-target arm_v8_1m_mve_ok } */
/* { dg-add-options arm_v8_1m_mve } */
/* { dg-additional-options "-O2" } */

#include "arm_mve.h"

int32x4x2_t
foo (int32_t const * addr)
{
  return vld2q_s32 (addr);
}

/* { dg-final { scan-assembler "vld20.32"  }  } */
/* { dg-final { scan-assembler "vld21.32"  }  } */

int32x4x2_t
foo1 (int32_t const * addr)
{
  return vld2q (addr);
}

/* { dg-final { scan-assembler "vld20.32"  }  } */
