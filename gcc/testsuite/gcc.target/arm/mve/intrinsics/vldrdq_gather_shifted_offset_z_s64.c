/* { dg-require-effective-target arm_v8_1m_mve_ok } */
/* { dg-add-options arm_v8_1m_mve } */
/* { dg-additional-options "-O2" } */

#include "arm_mve.h"

int64x2_t
foo (int64_t const * base, uint64x2_t offset, mve_pred16_t p)
{
  return vldrdq_gather_shifted_offset_z_s64 (base, offset, p);
}

/* { dg-final { scan-assembler "vldrdt.u64"  }  } */

int64x2_t
foo1 (int64_t const * base, uint64x2_t offset, mve_pred16_t p)
{
  return vldrdq_gather_shifted_offset_z (base, offset, p);
}

/* { dg-final { scan-assembler "vldrdt.u64"  }  } */
