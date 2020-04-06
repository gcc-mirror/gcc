/* { dg-require-effective-target arm_v8_1m_mve_ok } */
/* { dg-add-options arm_v8_1m_mve } */
/* { dg-additional-options "-O2" } */

#include "arm_mve.h"

void
foo (uint32x4_t * addr, uint32x4_t value)
{
  vstrwq_scatter_base_wb_u32 (addr, 8, value);
}

/* { dg-final { scan-assembler "vstrw.u32"  }  } */

void
foo1 (uint32x4_t * addr, uint32x4_t value)
{
  vstrwq_scatter_base_wb (addr, 8, value);
}

/* { dg-final { scan-assembler "vstrw.u32"  }  } */
