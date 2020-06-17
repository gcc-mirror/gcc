/* { dg-require-effective-target arm_v8_1m_mve_ok } */
/* { dg-add-options arm_v8_1m_mve } */
/* { dg-additional-options "-O2" } */

#include "arm_mve.h"

void
foo (uint64_t * base, uint64x2_t offset, uint64x2_t value)
{
  vstrdq_scatter_offset_u64 (base, offset, value);
}

/* { dg-final { scan-assembler "vstrd.64"  }  } */

void
foo1 (uint64_t * base, uint64x2_t offset, uint64x2_t value)
{
  vstrdq_scatter_offset (base, offset, value);
}

/* { dg-final { scan-assembler "vstrd.64"  }  } */
