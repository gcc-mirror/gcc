/* { dg-require-effective-target arm_v8_1m_mve_ok } */
/* { dg-add-options arm_v8_1m_mve } */
/* { dg-additional-options "-O2" } */

#include "arm_mve.h"

uint16x8_t
foo (uint8_t const * base, uint16x8_t offset)
{
  return vldrbq_gather_offset_u16 (base, offset);
}

/* { dg-final { scan-assembler "vldrb.u16"  }  } */

uint16x8_t
foo1 (uint8_t const * base, uint16x8_t offset)
{
  return vldrbq_gather_offset (base, offset);
}

/* { dg-final { scan-assembler "vldrb.u16"  }  } */
