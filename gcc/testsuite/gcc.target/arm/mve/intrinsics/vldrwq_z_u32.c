/* { dg-require-effective-target arm_v8_1m_mve_ok } */
/* { dg-add-options arm_v8_1m_mve } */
/* { dg-additional-options "-O2" } */

#include "arm_mve.h"

uint32x4_t
foo (uint32_t const * base, mve_pred16_t p)
{
  return vldrwq_z_u32 (base, p);
}

/* { dg-final { scan-assembler-times "vpst" 1 }  } */
/* { dg-final { scan-assembler-times "vldrwt.32" 1 }  } */
/* { dg-final { scan-assembler-not "__ARM_undef" } } */
