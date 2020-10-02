/* { dg-require-effective-target arm_v8_1m_mve_ok } */
/* { dg-add-options arm_v8_1m_mve } */
/* { dg-additional-options "-O2" } */

#include "arm_mve.h"

int16_t
foo (int16_t a, int16x8_t b, mve_pred16_t p)
{
  return vminvq_p_s16 (a, b, p);
}


int16_t
foo1 (int16_t a, int16x8_t b, mve_pred16_t p)
{
  return vminvq_p (a, b, p);
}


int16_t
foo2 (int8_t a, int16x8_t b, mve_pred16_t p)
{
  return vminvq_p (a, b, p);
}

/* { dg-final { scan-assembler-not "__ARM_undef" } } */
/* { dg-final { scan-assembler-times "vminvt.s16" 3 } } */
