/* { dg-require-effective-target arm_v8_1m_mve_ok } */
/* { dg-add-options arm_v8_1m_mve } */
/* { dg-additional-options "-O2" } */

#include "arm_mve.h"

int64x2_t
foo (uint64x2_t addr)
{
  return vldrdq_gather_base_s64 (addr, 8);
}

/* { dg-final { scan-assembler "vldrd.64"  }  } */
