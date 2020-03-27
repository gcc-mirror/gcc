/* { dg-require-effective-target arm_v8_1m_mve_ok } */
/* { dg-add-options arm_v8_1m_mve } */
/* { dg-additional-options "-O2" } */

#include "arm_mve.h"

int32x4_t
foo (int32_t const * base, uint32x4_t offset, mve_pred16_t p)
{
  return vldrwq_gather_offset_z_s32 (base, offset, p);
}

/* { dg-final { scan-assembler "vldrwt.u32"  }  } */

int32x4_t
foo1 (int32_t const * base, uint32x4_t offset, mve_pred16_t p)
{
  return vldrwq_gather_offset_z (base, offset, p);
}

/* { dg-final { scan-assembler "vldrwt.u32"  }  } */
