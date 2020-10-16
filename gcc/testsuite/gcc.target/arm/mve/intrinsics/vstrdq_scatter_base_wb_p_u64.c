/* { dg-require-effective-target arm_v8_1m_mve_ok } */
/* { dg-add-options arm_v8_1m_mve } */
/* { dg-additional-options "-O2" } */

#include "arm_mve.h"

void
foo (uint64x2_t * addr, const int offset, uint64x2_t value, mve_pred16_t p)
{
  vstrdq_scatter_base_wb_p_u64 (addr, 8, value, p);
}

void
foo1 (uint64x2_t * addr, const int offset, uint64x2_t value, mve_pred16_t p)
{
  vstrdq_scatter_base_wb_p (addr, 8, value, p);
}

/* { dg-final { scan-assembler-times "vstrdt.u64\tq\[0-9\]+, \\\[q\[0-9\]+, #\[0-9\]+\\\]!" 2 } } */
