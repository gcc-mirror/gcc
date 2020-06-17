/* { dg-require-effective-target arm_v8_1m_mve_ok } */
/* { dg-add-options arm_v8_1m_mve } */
/* { dg-additional-options "-O2" } */

#include "arm_mve.h"

int16x8_t
foo (int8_t const * base, uint16x8_t offset)
{
  return vldrbq_gather_offset_s16 (base, offset);
}

/* { dg-final { scan-assembler "vldrb.s16"  }  } */

int16x8_t
foo1 (int8_t const * base, uint16x8_t offset)
{
  return vldrbq_gather_offset (base, offset);
}

/* { dg-final { scan-assembler "vldrb.s16"  }  } */
