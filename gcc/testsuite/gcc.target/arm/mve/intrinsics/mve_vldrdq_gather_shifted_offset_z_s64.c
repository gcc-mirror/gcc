/* { dg-require-effective-target arm_v8_1m_mve_ok } */
/* { dg-add-options arm_v8_1m_mve } */
/* { dg-additional-options "-O2" } */

#include "arm_mve.h"
int64x2_t
foo1 (int64_t * base, uint64x2_t offset, mve_pred16_t p)
{
  return vldrdq_gather_shifted_offset_z (base, offset, p);
}

/* { dg-final { scan-assembler-not "__ARM_undef" } } */
