/* { dg-require-effective-target arm_v8_1m_mve_ok } */
/* { dg-add-options arm_v8_1m_mve } */
/* { dg-additional-options "-O2" } */

#include "arm_mve.h"

uint32x4_t
foo (uint32x4_t inactive, uint32x4_t a, mve_pred16_t p)
{
  return vrshrq_m_n_u32 (inactive, a, 32, p);
}

/* { dg-final { scan-assembler "vpst" } } */
/* { dg-final { scan-assembler "vrshrt.u32"  }  } */

uint32x4_t
foo1 (uint32x4_t inactive, uint32x4_t a, mve_pred16_t p)
{
  return vrshrq_m (inactive, a, 32, p);
}

/* { dg-final { scan-assembler "vpst" } } */
/* { dg-final { scan-assembler "vrshrt.u32"  }  } */
