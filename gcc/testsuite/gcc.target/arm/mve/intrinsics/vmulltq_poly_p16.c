/* { dg-require-effective-target arm_v8_1m_mve_ok } */
/* { dg-add-options arm_v8_1m_mve } */
/* { dg-additional-options "-O2" } */

#include "arm_mve.h"

uint32x4_t
foo (uint16x8_t a, uint16x8_t b)
{
  return vmulltq_poly_p16 (a, b);
}

/* { dg-final { scan-assembler "vmullt.p16"  }  } */

uint32x4_t
foo1 (uint16x8_t a, uint16x8_t b)
{
  return vmulltq_poly (a, b);
}

/* { dg-final { scan-assembler "vmullt.p16"  }  } */
