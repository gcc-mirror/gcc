/* { dg-require-effective-target arm_v8_1m_mve_ok } */
/* { dg-add-options arm_v8_1m_mve } */
/* { dg-additional-options "-O2" } */

#include "arm_mve.h"

int32x4_t
foo (uint32x4_t addr)
{
  return vldrwq_gather_base_s32 (addr, 4);
}

/* { dg-final { scan-assembler "vldrw.u32"  }  } */
