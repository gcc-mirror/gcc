/* { dg-require-effective-target arm_v8_1m_mve_ok } */
/* { dg-add-options arm_v8_1m_mve } */
/* { dg-additional-options "-O2" } */

#include "arm_mve.h"

uint32x4x2_t
foo (uint32_t const * addr)
{
  return vld2q_u32 (addr);
}

/* { dg-final { scan-assembler "vld20.32"  }  } */
/* { dg-final { scan-assembler "vld21.32"  }  } */

uint32x4x2_t
foo1 (uint32_t const * addr)
{
  return vld2q (addr);
}

/* { dg-final { scan-assembler "vld20.32"  }  } */
