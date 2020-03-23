/* { dg-require-effective-target arm_v8_1m_mve_ok } */
/* { dg-add-options arm_v8_1m_mve } */
/* { dg-additional-options "-O2" } */

#include "arm_mve.h"

uint8x16_t
foo (uint8x16_t a, int8x16_t b)
{
  return vqrshlq_u8 (a, b);
}

/* { dg-final { scan-assembler "vqrshl.u8"  }  } */

uint8x16_t
foo1 (uint8x16_t a, int8x16_t b)
{
  return vqrshlq (a, b);
}

/* { dg-final { scan-assembler "vqrshl.u8"  }  } */
