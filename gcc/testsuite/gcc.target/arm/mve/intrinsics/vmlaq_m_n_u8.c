/* { dg-require-effective-target arm_v8_1m_mve_ok } */
/* { dg-add-options arm_v8_1m_mve } */
/* { dg-additional-options "-O2" } */

#include "arm_mve.h"

uint8x16_t
foo (uint8x16_t a, uint8x16_t b, uint8_t c, mve_pred16_t p)
{
  return vmlaq_m_n_u8 (a, b, c, p);
}

/* { dg-final { scan-assembler "vpst" } } */
/* { dg-final { scan-assembler "vmlat.u8"  }  } */

uint8x16_t
foo1 (uint8x16_t a, uint8x16_t b, uint8_t c, mve_pred16_t p)
{
  return vmlaq_m (a, b, c, p);
}

/* { dg-final { scan-assembler "vpst" } } */
/* { dg-final { scan-assembler "vmlat.u8"  }  } */
