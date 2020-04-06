/* { dg-require-effective-target arm_v8_1m_mve_ok } */
/* { dg-add-options arm_v8_1m_mve } */
/* { dg-additional-options "-O2" } */

#include "arm_mve.h"

uint32x4_t
foo (uint8_t const * base, uint32x4_t offset, mve_pred16_t p)
{
  return vldrbq_gather_offset_z_u32 (base, offset, p);
}

/* { dg-final { scan-assembler "vldrbt.u32"  }  } */

uint32x4_t
foo1 (uint8_t const * base, uint32x4_t offset, mve_pred16_t p)
{
  return vldrbq_gather_offset_z (base, offset, p);
}

/* { dg-final { scan-assembler "vldrbt.u32"  }  } */
