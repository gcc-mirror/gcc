/* { dg-require-effective-target arm_v8_1m_mve_ok } */
/* { dg-add-options arm_v8_1m_mve } */
/* { dg-additional-options "-O2" } */

#include "arm_mve.h"

uint32x4_t
foo (uint32x4_t a, mve_pred16_t p)
{
  return vclzq_x_u32 (a, p);
}

/* { dg-final { scan-assembler "vpst" } } */
/* { dg-final { scan-assembler "vclzt.i32"  }  } */

uint32x4_t
foo1 (uint32x4_t a, mve_pred16_t p)
{
  return vclzq_x (a, p);
}

/* { dg-final { scan-assembler "vpst" } } */
