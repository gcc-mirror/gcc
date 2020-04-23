/* { dg-require-effective-target arm_v8_1m_mve_fp_ok } */
/* { dg-add-options arm_v8_1m_mve_fp } */
/* { dg-additional-options "-O2" } */

#include "arm_mve.h"
float32x4_t
foo1 (float32_t * base, uint32x4_t offset)
{
  return vldrwq_gather_offset (base, offset);
}

/* { dg-final { scan-assembler-not "__ARM_undef" } } */
