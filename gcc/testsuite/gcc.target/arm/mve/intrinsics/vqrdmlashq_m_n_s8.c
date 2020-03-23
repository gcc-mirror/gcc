/* { dg-require-effective-target arm_v8_1m_mve_ok } */
/* { dg-add-options arm_v8_1m_mve } */
/* { dg-additional-options "-O2" } */

#include "arm_mve.h"

int8x16_t
foo (int8x16_t a, int8x16_t b, int8_t c, mve_pred16_t p)
{
  return vqrdmlashq_m_n_s8 (a, b, c, p);
}

/* { dg-final { scan-assembler "vpst" } } */
/* { dg-final { scan-assembler "vqrdmlasht.s8"  }  } */

int8x16_t
foo1 (int8x16_t a, int8x16_t b, int8_t c, mve_pred16_t p)
{
  return vqrdmlashq_m (a, b, c, p);
}

/* { dg-final { scan-assembler "vpst" } } */
/* { dg-final { scan-assembler "vqrdmlasht.s8"  }  } */
