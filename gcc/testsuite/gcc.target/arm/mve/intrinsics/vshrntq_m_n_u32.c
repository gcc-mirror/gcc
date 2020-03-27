/* { dg-require-effective-target arm_v8_1m_mve_ok } */
/* { dg-add-options arm_v8_1m_mve } */
/* { dg-additional-options "-O2" } */

#include "arm_mve.h"

uint16x8_t
foo (uint16x8_t a, uint32x4_t b, mve_pred16_t p)
{
  return vshrntq_m_n_u32 (a, b, 16, p);
}

/* { dg-final { scan-assembler "vpst" } } */
/* { dg-final { scan-assembler "vshrntt.i32"  }  } */

uint16x8_t
foo1 (uint16x8_t a, uint32x4_t b, mve_pred16_t p)
{
  return vshrntq_m (a, b, 16, p);
}

/* { dg-final { scan-assembler "vpst" } } */
/* { dg-final { scan-assembler "vshrntt.i32"  }  } */
