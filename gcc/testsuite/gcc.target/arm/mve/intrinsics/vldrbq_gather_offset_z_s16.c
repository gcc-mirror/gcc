/* { dg-require-effective-target arm_v8_1m_mve_ok } */
/* { dg-add-options arm_v8_1m_mve } */
/* { dg-additional-options "-O2" } */

#include "arm_mve.h"

int16x8_t
foo (int8_t const * base, uint16x8_t offset, mve_pred16_t p)
{
  return vldrbq_gather_offset_z_s16 (base, offset, p);
}

/* { dg-final { scan-assembler "vldrbt.s16"  }  } */

int16x8_t
foo1 (int8_t const * base, uint16x8_t offset, mve_pred16_t p)
{
  return vldrbq_gather_offset_z (base, offset, p);
}

/* { dg-final { scan-assembler "vldrbt.s16"  }  } */
