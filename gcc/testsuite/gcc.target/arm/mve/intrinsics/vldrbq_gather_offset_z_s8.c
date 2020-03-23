/* { dg-require-effective-target arm_v8_1m_mve_ok } */
/* { dg-add-options arm_v8_1m_mve } */
/* { dg-additional-options "-O2" } */

#include "arm_mve.h"

int8x16_t
foo (int8_t const * base, uint8x16_t offset, mve_pred16_t p)
{
  return vldrbq_gather_offset_z_s8 (base, offset, p);
}

/* { dg-final { scan-assembler "vldrbt.u8"  }  } */

int8x16_t
foo1 (int8_t const * base, uint8x16_t offset, mve_pred16_t p)
{
  return vldrbq_gather_offset_z (base, offset, p);
}

/* { dg-final { scan-assembler "vldrbt.u8"  }  } */
