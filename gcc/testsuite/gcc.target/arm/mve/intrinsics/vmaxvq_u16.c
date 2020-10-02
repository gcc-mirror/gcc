/* { dg-require-effective-target arm_v8_1m_mve_ok } */
/* { dg-add-options arm_v8_1m_mve } */
/* { dg-additional-options "-O2" } */

#include "arm_mve.h"

uint16_t
foo (uint16_t a, uint16x8_t b)
{
  return vmaxvq_u16 (a, b);
}


uint16_t
foo1 (uint16_t a, uint16x8_t b)
{
  return vmaxvq (a, b);
}


uint16_t
foo2 (uint32_t a, uint16x8_t b)
{
  return vmaxvq (a, b);
}

/* { dg-final { scan-assembler-not "__ARM_undef" } } */
/* { dg-final { scan-assembler-times "vmaxv.u16" 3 } } */
