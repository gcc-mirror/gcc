/* { dg-require-effective-target arm_v8_1m_mve_ok } */
/* { dg-add-options arm_v8_1m_mve } */
/* { dg-additional-options "-O2" } */

#include "arm_mve.h"

uint8x16_t
foo (uint8x16_t a, uint8x16_t b, uint8_t c)
{
  return vqdmlahq_n_u8 (a, b, c);
}

/* { dg-final { scan-assembler "vqdmlah.s8"  }  } */

uint8x16_t
foo1 (uint8x16_t a, uint8x16_t b, uint8_t c)
{
  return vqdmlahq (a, b, c);
}

/* { dg-final { scan-assembler "vqdmlah.s8"  }  } */
