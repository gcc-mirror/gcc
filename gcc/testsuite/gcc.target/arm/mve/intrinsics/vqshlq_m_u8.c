/* { dg-require-effective-target arm_v8_1m_mve_ok } */
/* { dg-add-options arm_v8_1m_mve } */
/* { dg-additional-options "-O2" } */

#include "arm_mve.h"

uint8x16_t
foo (uint8x16_t inactive, uint8x16_t a, int8x16_t b, mve_pred16_t p)
{
  return vqshlq_m_u8 (inactive, a, b, p);
}

/* { dg-final { scan-assembler "vpst" } } */
/* { dg-final { scan-assembler "vqshlt.u8"  }  } */

uint8x16_t
foo1 (uint8x16_t inactive, uint8x16_t a, int8x16_t b, mve_pred16_t p)
{
  return vqshlq_m (inactive, a, b, p);
}

/* { dg-final { scan-assembler "vpst" } } */
/* { dg-final { scan-assembler "vqshlt.u8"  }  } */
