/* { dg-require-effective-target arm_v8_1m_mve_ok } */
/* { dg-add-options arm_v8_1m_mve } */
/* { dg-additional-options "-O2" } */

#include "arm_mve.h"

uint32x4_t
foo (uint32x4_t inactive, int32x4_t a, mve_pred16_t p)
{
  return vqshluq_m_n_s32 (inactive, a, 7, p);
}

/* { dg-final { scan-assembler "vpst" } } */
/* { dg-final { scan-assembler "vqshlut.s32"  }  } */

uint32x4_t
foo1 (uint32x4_t inactive, int32x4_t a, mve_pred16_t p)
{
  return vqshluq_m (inactive, a, 7, p);
}

/* { dg-final { scan-assembler "vpst" } } */
