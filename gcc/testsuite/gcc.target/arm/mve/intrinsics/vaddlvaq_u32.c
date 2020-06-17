/* { dg-require-effective-target arm_v8_1m_mve_ok } */
/* { dg-add-options arm_v8_1m_mve } */
/* { dg-additional-options "-O2" } */

#include "arm_mve.h"

uint64_t
foo (uint64_t a, uint32x4_t b)
{
  return vaddlvaq_u32 (a, b);
}

/* { dg-final { scan-assembler "vaddlva.u32"  }  } */

uint64_t
foo1 (uint64_t a, uint32x4_t b)
{
  return vaddlvaq (a, b);
}

/* { dg-final { scan-assembler "vaddlva.u32"  }  } */
