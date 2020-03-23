/* { dg-require-effective-target arm_v8_1m_mve_ok } */
/* { dg-add-options arm_v8_1m_mve } */
/* { dg-additional-options "-O2" } */

#include "arm_mve.h"

void
foo (uint32x4_t addr, int32x4_t value, mve_pred16_t p)
{
  vstrwq_scatter_base_p_s32 (addr, 8, value, p);
}

/* { dg-final { scan-assembler "vstrwt.u32"  }  } */

void
foo1 (uint32x4_t addr, int32x4_t value, mve_pred16_t p)
{
  vstrwq_scatter_base_p (addr, 8, value, p);
}

/* { dg-final { scan-assembler "vstrwt.u32"  }  } */
