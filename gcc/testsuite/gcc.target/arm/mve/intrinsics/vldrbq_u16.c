/* { dg-require-effective-target arm_v8_1m_mve_ok } */
/* { dg-add-options arm_v8_1m_mve } */
/* { dg-additional-options "-O2" } */

#include "arm_mve.h"

uint16x8_t
foo (uint8_t const * base)
{
  return vldrbq_u16 (base);
}

/* { dg-final { scan-assembler "vldrb.u16"  }  } */
