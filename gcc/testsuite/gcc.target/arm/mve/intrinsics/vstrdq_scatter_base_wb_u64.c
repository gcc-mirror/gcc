/* { dg-require-effective-target arm_v8_1m_mve_ok } */
/* { dg-add-options arm_v8_1m_mve } */
/* { dg-additional-options "-O2" } */

#include "arm_mve.h"

void
foo (uint64x2_t * addr, const int offset, uint64x2_t value)
{
  vstrdq_scatter_base_wb_u64 (addr, 8, value);
}

/* { dg-final { scan-assembler "vstrd.u64"  }  } */

void
foo1 (uint64x2_t * addr, const int offset, uint64x2_t value)
{
  vstrdq_scatter_base_wb (addr, 8, value);
}

/* { dg-final { scan-assembler "vstrd.u64"  }  } */
