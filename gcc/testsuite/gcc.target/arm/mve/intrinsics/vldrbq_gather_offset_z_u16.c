/* { dg-require-effective-target arm_v8_1m_mve_ok } */
/* { dg-add-options arm_v8_1m_mve } */
/* { dg-additional-options "-O2" } */

#include "arm_mve.h"

uint16x8_t
foo (uint8_t const * base, uint16x8_t offset, mve_pred16_t p)
{
  return vldrbq_gather_offset_z_u16 (base, offset, p);
}

/* { dg-final { scan-assembler "vldrbt.u16"  }  } */

uint16x8_t
foo1 (uint8_t const * base, uint16x8_t offset, mve_pred16_t p)
{
  return vldrbq_gather_offset_z (base, offset, p);
}

/* { dg-final { scan-assembler "vldrbt.u16"  }  } */
