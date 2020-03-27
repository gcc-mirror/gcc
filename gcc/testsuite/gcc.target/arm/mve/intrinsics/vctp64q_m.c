/* { dg-require-effective-target arm_v8_1m_mve_ok } */
/* { dg-add-options arm_v8_1m_mve } */
/* { dg-additional-options "-O2" } */

#include "arm_mve.h"

mve_pred16_t
foo (uint32_t a, mve_pred16_t p)
{
  return vctp64q_m (a, p);
}

/* { dg-final { scan-assembler "vpst" } } */
/* { dg-final { scan-assembler "vctpt.64"  }  } */

mve_pred16_t
foo1 (uint32_t a, mve_pred16_t p)
{
  return vctp64q_m (a, p);
}

/* { dg-final { scan-assembler "vpst" } } */
