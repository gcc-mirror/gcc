/* { dg-require-effective-target arm_v8_1m_mve_ok } */
/* { dg-add-options arm_v8_1m_mve } */
/* { dg-additional-options "-O2" } */

#include "arm_mve.h"

uint16x8_t
foo (uint16x8_t inactive, uint8x16_t a, uint8x16_t b, mve_pred16_t p)
{
  return vmulltq_poly_m_p8 (inactive, a, b, p);
}

/* { dg-final { scan-assembler "vpst" } } */
/* { dg-final { scan-assembler "vmulltt.p8"  }  } */

uint16x8_t
foo1 (uint16x8_t inactive, uint8x16_t a, uint8x16_t b, mve_pred16_t p)
{
  return vmulltq_poly_m (inactive, a, b, p);
}

/* { dg-final { scan-assembler "vpst" } } */
/* { dg-final { scan-assembler "vmulltt.p8"  }  } */
