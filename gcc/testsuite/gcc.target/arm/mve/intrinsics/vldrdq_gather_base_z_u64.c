/* { dg-require-effective-target arm_v8_1m_mve_ok } */
/* { dg-add-options arm_v8_1m_mve } */
/* { dg-additional-options "-O2" } */

#include "arm_mve.h"

uint64x2_t
foo (uint64x2_t addr, mve_pred16_t p)
{
  return vldrdq_gather_base_z_u64 (addr, 8, p);
}

/* { dg-final { scan-assembler "vldrdt.u64"  }  } */
