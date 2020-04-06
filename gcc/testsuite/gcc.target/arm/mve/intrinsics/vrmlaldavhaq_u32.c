/* { dg-require-effective-target arm_v8_1m_mve_ok } */
/* { dg-add-options arm_v8_1m_mve } */
/* { dg-additional-options "-O2" } */

#include "arm_mve.h"

uint64_t
foo (uint64_t a, uint32x4_t b, uint32x4_t c)
{
  return vrmlaldavhaq_u32 (a, b, c);
}

/* { dg-final { scan-assembler "vrmlaldavha.u32"  }  } */

uint64_t
foo1 (uint64_t a, uint32x4_t b, uint32x4_t c)
{
  return vrmlaldavhaq (a, b, c);
}

/* { dg-final { scan-assembler "vrmlaldavha.u32"  }  } */
