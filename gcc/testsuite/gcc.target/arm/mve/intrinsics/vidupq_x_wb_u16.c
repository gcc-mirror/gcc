/* { dg-require-effective-target arm_v8_1m_mve_ok } */
/* { dg-add-options arm_v8_1m_mve } */
/* { dg-additional-options "-O2" } */

#include "arm_mve.h"

uint32_t *a;

uint16x8_t
foo (mve_pred16_t p)
{
  return vidupq_x_wb_u16 (a, 8, p);
}

/* { dg-final { scan-assembler "vpst" } } */
/* { dg-final { scan-assembler "vidupt.u16"  }  } */

uint16x8_t
foo1 (mve_pred16_t p)
{
  return vidupq_x_u16 (a, 8, p);
}

/* { dg-final { scan-assembler "vpst" } } */
/* { dg-final { scan-assembler "vidupt.u16"  }  } */
