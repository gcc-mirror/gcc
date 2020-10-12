/* { dg-require-effective-target arm_v8_1m_mve_ok } */
/* { dg-add-options arm_v8_1m_mve } */
/* { dg-additional-options "-O2" } */

#include "arm_mve.h"

uint16_t
foo (uint16_t a, int16x8_t b)
{
  return vminavq_s16 (a, b);
}


uint16_t
foo1 (uint16_t a, int16x8_t b)
{
  return vminavq (a, b);
}


int16_t
foo2 (uint8_t a, int16x8_t b)
{
  return vminavq (a, b);
}

/* { dg-final { scan-assembler-not "__ARM_undef" } } */
/* { dg-final { scan-assembler-times "vminav.s16" 3 } } */
