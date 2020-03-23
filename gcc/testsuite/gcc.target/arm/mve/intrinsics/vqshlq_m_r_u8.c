/* { dg-require-effective-target arm_v8_1m_mve_ok } */
/* { dg-add-options arm_v8_1m_mve } */
/* { dg-additional-options "-O2" } */

#include "arm_mve.h"

uint8x16_t
foo (uint8x16_t a, int32_t b, mve_pred16_t p)
{
  return vqshlq_m_r_u8 (a, b, p);
}

/* { dg-final { scan-assembler "vpst" } } */
/* { dg-final { scan-assembler "vqshlt.u8"  }  } */

uint8x16_t
foo1 (uint8x16_t a, int32_t b, mve_pred16_t p)
{
  return vqshlq_m_r (a, b, p);
}

/* { dg-final { scan-assembler "vpst" } } */
