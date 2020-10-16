/* { dg-require-effective-target arm_v8_1m_mve_ok } */
/* { dg-add-options arm_v8_1m_mve } */
/* { dg-additional-options "-O2" } */

#include "arm_mve.h"

void
foo (uint64x2_t * addr, const int offset, int64x2_t value)
{
  vstrdq_scatter_base_wb_s64 (addr, 8, value);
}

void
foo1 (uint64x2_t * addr, const int offset, int64x2_t value)
{
  vstrdq_scatter_base_wb (addr, 8, value);
}

/* { dg-final { scan-assembler-times "vstrd.u64\tq\[0-9\]+, \\\[q\[0-9\]+, #\[0-9\]+\\\]!" 2 } } */
