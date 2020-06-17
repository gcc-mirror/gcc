/* { dg-require-effective-target arm_v8_1m_mve_ok } */
/* { dg-add-options arm_v8_1m_mve } */
/* { dg-additional-options "-O2" } */

#include "arm_mve.h"

void
foo (uint64x2_t addr, const int offset, uint64x2_t value, mve_pred16_t p)
{
  vstrdq_scatter_base_p_u64 (addr, 8, value, p);
}

/* { dg-final { scan-assembler "vstrdt.u64"  }  } */

void
foo1 (uint64x2_t addr, const int offset, uint64x2_t value, mve_pred16_t p)
{
  vstrdq_scatter_base_p (addr, 8, value, p);
}

/* { dg-final { scan-assembler "vstrdt.u64"  }  } */
