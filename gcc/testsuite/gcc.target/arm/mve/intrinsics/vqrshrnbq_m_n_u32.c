/* { dg-require-effective-target arm_v8_1m_mve_ok } */
/* { dg-add-options arm_v8_1m_mve } */
/* { dg-additional-options "-O2" } */

#include "arm_mve.h"

uint16x8_t
foo (uint16x8_t a, uint32x4_t b, mve_pred16_t p)
{
  return vqrshrnbq_m_n_u32 (a, b, 1, p);
}

/* { dg-final { scan-assembler "vpst" } } */
/* { dg-final { scan-assembler "vqrshrnbt.u32"  }  } */

uint16x8_t
foo1 (uint16x8_t a, uint32x4_t b, mve_pred16_t p)
{
  return vqrshrnbq_m (a, b, 1, p);
}

/* { dg-final { scan-assembler "vpst" } } */
/* { dg-final { scan-assembler "vqrshrnbt.u32"  }  } */
