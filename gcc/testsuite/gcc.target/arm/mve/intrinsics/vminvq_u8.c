/* { dg-require-effective-target arm_v8_1m_mve_ok } */
/* { dg-add-options arm_v8_1m_mve } */
/* { dg-additional-options "-O2" } */

#include "arm_mve.h"

uint8_t
foo (uint8_t a, uint8x16_t b)
{
  return vminvq_u8 (a, b);
}


uint8_t
foo1 (uint8_t a, uint8x16_t b)
{
  return vminvq (a, b);
}


uint16_t
foo2 (uint32_t a, uint8x16_t b)
{
  return vminvq (a, b);
}

/* { dg-final { scan-assembler-not "__ARM_undef" } } */
/* { dg-final { scan-assembler-times "vminv.u8" 3 } } */
