/* { dg-require-effective-target arm_v8_1m_mve_ok } */
/* { dg-add-options arm_v8_1m_mve } */
/* { dg-additional-options "-O2" } */

#include "arm_mve.h"

uint8x16_t
foo (uint32_t *a, uint32_t b)
{
  return vdwdupq_wb_u8 (a, b, 4);
}

/* { dg-final { scan-assembler "vdwdup.u8"  }  } */

uint8x16_t
foo1 (uint32_t *a, uint32_t b)
{
  return vdwdupq_u8 (a, b, 4);
}

/* { dg-final { scan-assembler "vdwdup.u8"  }  } */
