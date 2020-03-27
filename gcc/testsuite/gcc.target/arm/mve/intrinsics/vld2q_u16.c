/* { dg-require-effective-target arm_v8_1m_mve_ok } */
/* { dg-add-options arm_v8_1m_mve } */
/* { dg-additional-options "-O2" } */

#include "arm_mve.h"

uint16x8x2_t
foo (uint16_t const * addr)
{
  return vld2q_u16 (addr);
}

/* { dg-final { scan-assembler "vld20.16"  }  } */
/* { dg-final { scan-assembler "vld21.16"  }  } */

uint16x8x2_t
foo1 (uint16_t const * addr)
{
  return vld2q (addr);
}

/* { dg-final { scan-assembler "vld20.16"  }  } */
