/* { dg-require-effective-target arm_v8_1m_mve_ok } */
/* { dg-add-options arm_v8_1m_mve } */
/* { dg-additional-options "-O2" } */

#include "arm_mve.h"

uint32x4_t
foo (uint32_t * a, uint32_t b)
{
  return viwdupq_wb_u32 (a, b, 8);
}

/* { dg-final { scan-assembler "viwdup.u32"  }  } */

uint32x4_t
foo1 (uint32_t * a, uint32_t b)
{
  return viwdupq_u32 (a, b, 8);
}

/* { dg-final { scan-assembler "viwdup.u32"  }  } */
