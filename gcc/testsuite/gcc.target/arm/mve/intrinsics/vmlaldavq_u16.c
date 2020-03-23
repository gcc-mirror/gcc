/* { dg-require-effective-target arm_v8_1m_mve_ok } */
/* { dg-add-options arm_v8_1m_mve } */
/* { dg-additional-options "-O2" } */

#include "arm_mve.h"

uint64_t
foo (uint16x8_t a, uint16x8_t b)
{
  return vmlaldavq_u16 (a, b);
}

/* { dg-final { scan-assembler "vmlaldav.u16"  }  } */

uint64_t
foo1 (uint16x8_t a, uint16x8_t b)
{
  return vmlaldavq (a, b);
}

/* { dg-final { scan-assembler "vmlaldav.u16"  }  } */
