/* { dg-require-effective-target arm_v8_1m_mve_ok } */
/* { dg-add-options arm_v8_1m_mve } */
/* { dg-additional-options "-O2" } */

#include "arm_mve.h"

void
foo (uint32_t * base, uint32x4_t offset, uint32x4_t value)
{
  vstrwq_scatter_offset_u32 (base, offset, value);
}

/* { dg-final { scan-assembler "vstrw.32"  }  } */

void
foo1 (uint32_t * base, uint32x4_t offset, uint32x4_t value)
{
  vstrwq_scatter_offset (base, offset, value);
}

/* { dg-final { scan-assembler "vstrw.32"  }  } */
