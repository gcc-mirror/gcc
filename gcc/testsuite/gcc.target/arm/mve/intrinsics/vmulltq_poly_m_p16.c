/* { dg-require-effective-target arm_v8_1m_mve_ok } */
/* { dg-add-options arm_v8_1m_mve } */
/* { dg-additional-options "-O2" } */

#include "arm_mve.h"

uint32x4_t
foo (uint32x4_t inactive, uint16x8_t a, uint16x8_t b, mve_pred16_t p)
{
  return vmulltq_poly_m_p16 (inactive, a, b, p);
}

/* { dg-final { scan-assembler "vpst" } } */
/* { dg-final { scan-assembler "vmulltt.p16"  }  } */

uint32x4_t
foo1 (uint32x4_t inactive, uint16x8_t a, uint16x8_t b, mve_pred16_t p)
{
  return vmulltq_poly_m (inactive, a, b, p);
}

/* { dg-final { scan-assembler "vpst" } } */
/* { dg-final { scan-assembler "vmulltt.p16"  }  } */
