/* { dg-require-effective-target arm_v8_1m_mve_ok } */
/* { dg-add-options arm_v8_1m_mve } */
/* { dg-additional-options "-O2" } */

#include "arm_mve.h"

int16x8_t
foo (int16x8_t a, int32x4_t b, mve_pred16_t p)
{
  return vqrshrntq_m_n_s32 (a, b, 1, p);
}

/* { dg-final { scan-assembler "vpst" } } */
/* { dg-final { scan-assembler "vqrshrntt.s32"  }  } */

int16x8_t
foo1 (int16x8_t a, int32x4_t b, mve_pred16_t p)
{
  return vqrshrntq_m (a, b, 1, p);
}

/* { dg-final { scan-assembler "vpst" } } */
/* { dg-final { scan-assembler "vqrshrntt.s32"  }  } */
