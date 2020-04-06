/* { dg-require-effective-target arm_v8_1m_mve_ok } */
/* { dg-add-options arm_v8_1m_mve } */
/* { dg-additional-options "-O2" } */

#include "arm_mve.h"

uint8x16_t
foo (uint8x16_t a)
{
  return vrshrq_n_u8 (a, 8);
}

/* { dg-final { scan-assembler "vrshr.u8"  }  } */

uint8x16_t
foo1 (uint8x16_t a)
{
  return vrshrq (a, 8);
}

/* { dg-final { scan-assembler "vrshr.u8"  }  } */
