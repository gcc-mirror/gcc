/* { dg-require-effective-target arm_v8_1m_mve_ok } */
/* { dg-add-options arm_v8_1m_mve } */
/* { dg-additional-options "-O2" } */

#include "arm_mve.h"

uint32x4_t
foo (uint32_t *a)
{
  return vddupq_wb_u32 (a, 1);
}

/* { dg-final { scan-assembler "vddup.u32"  }  } */

uint32x4_t
foo1 (uint32_t *a)
{
  return vddupq_u32 (a, 1);
}

/* { dg-final { scan-assembler "vddup.u32"  }  } */
