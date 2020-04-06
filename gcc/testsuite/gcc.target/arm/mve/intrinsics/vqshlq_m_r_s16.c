/* { dg-require-effective-target arm_v8_1m_mve_ok } */
/* { dg-add-options arm_v8_1m_mve } */
/* { dg-additional-options "-O2" } */

#include "arm_mve.h"

int16x8_t
foo (int16x8_t a, int32_t b, mve_pred16_t p)
{
  return vqshlq_m_r_s16 (a, b, p);
}

/* { dg-final { scan-assembler "vpst" } } */
/* { dg-final { scan-assembler "vqshlt.s16"  }  } */

int16x8_t
foo1 (int16x8_t a, int32_t b, mve_pred16_t p)
{
  return vqshlq_m_r (a, b, p);
}

/* { dg-final { scan-assembler "vpst" } } */
