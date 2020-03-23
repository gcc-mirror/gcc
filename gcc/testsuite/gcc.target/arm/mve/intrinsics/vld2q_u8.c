/* { dg-require-effective-target arm_v8_1m_mve_ok } */
/* { dg-add-options arm_v8_1m_mve } */
/* { dg-additional-options "-O2" } */

#include "arm_mve.h"

uint8x16x2_t
foo (uint8_t const * addr)
{
  return vld2q_u8 (addr);
}

/* { dg-final { scan-assembler "vld20.8"  }  } */
/* { dg-final { scan-assembler "vld21.8"  }  } */

uint8x16x2_t
foo1 (uint8_t const * addr)
{
  return vld2q (addr);
}

/* { dg-final { scan-assembler "vld20.8"  }  } */
