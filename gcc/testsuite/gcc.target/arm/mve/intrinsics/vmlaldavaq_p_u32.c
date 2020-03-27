/* { dg-require-effective-target arm_v8_1m_mve_ok } */
/* { dg-add-options arm_v8_1m_mve } */
/* { dg-additional-options "-O2" } */

#include "arm_mve.h"

uint64_t
foo (uint64_t a, uint32x4_t b, uint32x4_t c, mve_pred16_t p)
{
  return vmlaldavaq_p_u32 (a, b, c, p);
}

/* { dg-final { scan-assembler "vmlaldavat.u32"  }  } */

uint64_t
foo1 (uint64_t a, uint32x4_t b, uint32x4_t c, mve_pred16_t p)
{
  return vmlaldavaq_p (a, b, c, p);
}

/* { dg-final { scan-assembler "vmlaldavat.u32"  }  } */
