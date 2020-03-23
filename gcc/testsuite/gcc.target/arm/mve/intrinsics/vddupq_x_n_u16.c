/* { dg-require-effective-target arm_v8_1m_mve_ok } */
/* { dg-add-options arm_v8_1m_mve } */
/* { dg-additional-options "-O2" } */

#include "arm_mve.h"

uint16x8_t
foo (uint32_t a, mve_pred16_t p)
{
  return vddupq_x_n_u16 (a, 1, p);
}

/* { dg-final { scan-assembler "vpst" } } */
/* { dg-final { scan-assembler "vddupt.u16"  }  } */

uint16x8_t
foo1 (uint32_t a, mve_pred16_t p)
{
  return vddupq_x_u16 (a, 1, p);
}

/* { dg-final { scan-assembler "vpst" } } */
/* { dg-final { scan-assembler "vddupt.u16"  }  } */
