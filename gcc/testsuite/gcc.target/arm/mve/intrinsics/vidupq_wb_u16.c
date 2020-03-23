/* { dg-require-effective-target arm_v8_1m_mve_ok } */
/* { dg-add-options arm_v8_1m_mve } */
/* { dg-additional-options "-O2" } */

#include "arm_mve.h"

uint16x8_t
foo (uint32_t *a)
{
  return vidupq_wb_u16 (a, 4);
}

/* { dg-final { scan-assembler "vidup.u16"  }  } */

uint16x8_t
foo1 (uint32_t *a)
{
  return vidupq_u16 (a, 4);
}

/* { dg-final { scan-assembler "vidup.u16"  }  } */
