/* { dg-require-effective-target arm_v8_1m_mve_ok } */
/* { dg-add-options arm_v8_1m_mve } */
/* { dg-additional-options "-O2" } */

#include "arm_mve.h"

uint64x2_t
foo (uint64x2_t * addr)
{
  return vldrdq_gather_base_wb_u64 (addr, 8);
}

/* { dg-final { scan-assembler "vldrb.8 q\[0-9\]+, \\\[r\[0-9\]+\\\]" } } */
/* { dg-final { scan-assembler "vldrd.64\tq\[0-9\]+, \\\[q\[0-9\]+, #\[0-9\]+\\\]!" } } */
/* { dg-final { scan-assembler "vstrb.8 q\[0-9\]+, \\\[r\[0-9\]+\\\]" } } */
