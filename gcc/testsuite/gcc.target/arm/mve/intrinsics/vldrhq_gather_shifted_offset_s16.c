/* { dg-require-effective-target arm_v8_1m_mve_ok } */
/* { dg-add-options arm_v8_1m_mve } */
/* { dg-additional-options "-O2" } */

#include "arm_mve.h"

int16x8_t
foo (int16_t const * base, uint16x8_t offset)
{
  return vldrhq_gather_shifted_offset_s16 (base, offset);
}

/* { dg-final { scan-assembler "vldrh.u16"  }  } */

int16x8_t
foo1 (int16_t const * base, uint16x8_t offset)
{
  return vldrhq_gather_shifted_offset (base, offset);
}

/* { dg-final { scan-assembler "vldrh.u16"  }  } */
