/* { dg-require-effective-target arm_v8_1m_mve_ok } */
/* { dg-add-options arm_v8_1m_mve } */
/* { dg-additional-options "-O2" } */

#include "arm_mve.h"

uint8x16_t
foo (uint8_t const * base)
{
  return vldrbq_u8 (base);
}

/* { dg-final { scan-assembler-times "vldrb.8" 1 }  } */
/* { dg-final { scan-assembler-not "__ARM_undef" } } */
