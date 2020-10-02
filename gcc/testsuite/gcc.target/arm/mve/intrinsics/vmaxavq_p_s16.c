/* { dg-require-effective-target arm_v8_1m_mve_ok } */
/* { dg-add-options arm_v8_1m_mve } */
/* { dg-additional-options "-O2" } */

#include "arm_mve.h"

uint16_t
foo (uint16_t a, int16x8_t b, mve_pred16_t p)
{
  return vmaxavq_p_s16 (a, b, p);
}


uint16_t
foo1 (uint16_t a, int16x8_t b, mve_pred16_t p)
{
  return vmaxavq_p (a, b, p);
}


int16_t
foo2 (uint8_t a, int16x8_t b, mve_pred16_t p)
{
  return vmaxavq_p (a, b, p);
}

/* { dg-final { scan-assembler-not "__ARM_undef" } } */
/* { dg-final { scan-assembler-times "vmaxavt.s16" 3 } } */
