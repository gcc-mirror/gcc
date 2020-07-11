/* { dg-require-effective-target arm_v8_1m_mve_ok } */
/* { dg-add-options arm_v8_1m_mve } */
/* { dg-additional-options "-O2" } */

#include "arm_mve.h"

uint16x8_t
foo1 (uint16_t * base, uint16x8_t offset)
{
  return vldrhq_gather_shifted_offset (base, offset);
}

/* { dg-final { scan-assembler-not "__ARM_undef" } } */
