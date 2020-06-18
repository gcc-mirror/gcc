/* { dg-require-effective-target arm_v8_1m_mve_ok } */
/* { dg-add-options arm_v8_1m_mve } */
/* { dg-additional-options "-O2" } */

#include "arm_mve.h"

uint32x4_t
foo (uint32_t const * base)
{
  return vldrwq_u32 (base);
}

/* { dg-final { scan-assembler-times "vldrw.32" 1 }  } */
/* { dg-final { scan-assembler-not "__ARM_undef" } } */
