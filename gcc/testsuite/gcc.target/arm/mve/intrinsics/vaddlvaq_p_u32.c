/* { dg-require-effective-target arm_v8_1m_mve_ok } */
/* { dg-add-options arm_v8_1m_mve } */
/* { dg-additional-options "-O2" } */

#include "arm_mve.h"

uint64_t
foo (uint64_t a, uint32x4_t b, mve_pred16_t p)
{
  return vaddlvaq_p_u32 (a, b, p);
}

/* { dg-final { scan-assembler "vaddlvat.u32"  }  } */

uint64_t
foo1 (uint64_t a, uint32x4_t b, mve_pred16_t p)
{
  return vaddlvaq_p (a, b, p);
}

/* { dg-final { scan-assembler "vaddlvat.u32"  }  } */
