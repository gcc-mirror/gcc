/* { dg-require-effective-target arm_v8_1m_mve_ok } */
/* { dg-add-options arm_v8_1m_mve } */
/* { dg-additional-options "-O2" } */

#include "arm_mve.h"

uint16x8_t
foo (uint16x8_t a, mve_pred16_t p)
{
  return vrev32q_x_u16 (a, p);
}

/* { dg-final { scan-assembler "vpst" } } */
/* { dg-final { scan-assembler "vrev32t.16"  }  } */

uint16x8_t
foo1 (uint16x8_t a, mve_pred16_t p)
{
  return vrev32q_x (a, p);
}

/* { dg-final { scan-assembler "vpst" } } */
