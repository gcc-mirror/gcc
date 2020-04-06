/* { dg-require-effective-target arm_v8_1m_mve_ok } */
/* { dg-add-options arm_v8_1m_mve } */
/* { dg-additional-options "-O2" } */

#include "arm_mve.h"

uint8x16_t
foo (uint32_t a, mve_pred16_t p)
{
  return vddupq_x_n_u8 (a, 4, p);
}

/* { dg-final { scan-assembler "vpst" } } */
/* { dg-final { scan-assembler "vddupt.u8"  }  } */

uint8x16_t
foo1 (uint32_t a, mve_pred16_t p)
{
  return vddupq_x_u8 (a, 4, p);
}

/* { dg-final { scan-assembler "vpst" } } */
/* { dg-final { scan-assembler "vddupt.u8"  }  } */
