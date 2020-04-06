/* { dg-require-effective-target arm_v8_1m_mve_ok } */
/* { dg-add-options arm_v8_1m_mve } */
/* { dg-additional-options "-O2" } */

#include "arm_mve.h"

uint8x16_t
foo (uint8x16_t a, mve_pred16_t p)
{
  return vrshrq_x_n_u8 (a, 8, p);
}

/* { dg-final { scan-assembler "vpst" } } */
/* { dg-final { scan-assembler "vrshrt.u8"  }  } */

uint8x16_t
foo1 (uint8x16_t a, mve_pred16_t p)
{
  return vrshrq_x (a, 8, p);
}

/* { dg-final { scan-assembler "vpst" } } */
/* { dg-final { scan-assembler "vrshrt.u8"  }  } */
