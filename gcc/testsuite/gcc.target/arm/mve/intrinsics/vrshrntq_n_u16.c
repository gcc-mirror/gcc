/* { dg-require-effective-target arm_v8_1m_mve_ok } */
/* { dg-add-options arm_v8_1m_mve } */
/* { dg-additional-options "-O2" } */

#include "arm_mve.h"

uint8x16_t
foo (uint8x16_t a, uint16x8_t b)
{
  return vrshrntq_n_u16 (a, b, 1);
}

/* { dg-final { scan-assembler "vrshrnt.i16"  }  } */

uint8x16_t
foo1 (uint8x16_t a, uint16x8_t b)
{
  return vrshrntq (a, b, 1);
}

/* { dg-final { scan-assembler "vrshrnt.i16"  }  } */
