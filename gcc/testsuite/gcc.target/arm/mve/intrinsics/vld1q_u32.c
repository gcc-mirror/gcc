/* { dg-require-effective-target arm_v8_1m_mve_ok } */
/* { dg-add-options arm_v8_1m_mve } */
/* { dg-additional-options "-O2" } */

#include "arm_mve.h"

uint32x4_t
foo (uint32_t const * base)
{
  return vld1q_u32 (base);
}

uint32x4_t
foo1 (uint32_t const * base)
{
  return vld1q (base);
}

/* { dg-final { scan-assembler-times "vldrw.32" 2 }  } */
/* { dg-final { scan-assembler-not "__ARM_undef" } } */
