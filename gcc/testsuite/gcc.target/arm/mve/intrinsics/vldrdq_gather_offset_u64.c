/* { dg-require-effective-target arm_v8_1m_mve_ok } */
/* { dg-add-options arm_v8_1m_mve } */
/* { dg-additional-options "-O2" } */

#include "arm_mve.h"

uint64x2_t
foo (uint64_t const * base, uint64x2_t offset)
{
  return vldrdq_gather_offset_u64 (base, offset);
}

/* { dg-final { scan-assembler "vldrd.u64"  }  } */

uint64x2_t
foo1 (uint64_t const * base, uint64x2_t offset)
{
  return vldrdq_gather_offset (base, offset);
}

/* { dg-final { scan-assembler "vldrd.u64"  }  } */
