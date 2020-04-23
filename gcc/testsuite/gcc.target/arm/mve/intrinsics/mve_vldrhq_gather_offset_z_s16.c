/* { dg-require-effective-target arm_v8_1m_mve_ok } */
/* { dg-add-options arm_v8_1m_mve } */
/* { dg-additional-options "-O2" } */

#include "arm_mve.h"
int16x8_t
foo1 (int16_t * base, uint16x8_t offset, mve_pred16_t p)
{
  return vldrhq_gather_offset_z (base, offset, p);
}

/* { dg-final { scan-assembler-not "__ARM_undef" } } */
