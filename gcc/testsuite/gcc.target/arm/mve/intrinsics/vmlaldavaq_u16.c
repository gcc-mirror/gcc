/* { dg-require-effective-target arm_v8_1m_mve_ok } */
/* { dg-add-options arm_v8_1m_mve } */
/* { dg-additional-options "-O2" } */

#include "arm_mve.h"

uint64_t
foo (uint64_t a, uint16x8_t b, uint16x8_t c)
{
  return vmlaldavaq_u16 (a, b, c);
}

/* { dg-final { scan-assembler "vmlaldava.u16"  }  } */

uint64_t
foo1 (uint64_t a, uint16x8_t b, uint16x8_t c)
{
  return vmlaldavaq (a, b, c);
}

/* { dg-final { scan-assembler "vmlaldava.u16"  }  } */
