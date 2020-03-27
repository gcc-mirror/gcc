/* { dg-require-effective-target arm_v8_1m_mve_ok } */
/* { dg-add-options arm_v8_1m_mve } */
/* { dg-additional-options "-O2" } */

#include "arm_mve.h"

int64x2_t
foo (int64_t const * base, uint64x2_t offset)
{
  return vldrdq_gather_offset_s64 (base, offset);
}

/* { dg-final { scan-assembler "vldrd.u64"  }  } */

int64x2_t
foo1 (int64_t const * base, uint64x2_t offset)
{
  return vldrdq_gather_offset (base, offset);
}

/* { dg-final { scan-assembler "vldrd.u64"  }  } */
