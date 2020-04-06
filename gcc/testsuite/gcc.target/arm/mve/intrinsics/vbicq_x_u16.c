/* { dg-require-effective-target arm_v8_1m_mve_ok } */
/* { dg-add-options arm_v8_1m_mve } */
/* { dg-additional-options "-O2" } */

#include "arm_mve.h"

uint16x8_t
foo (uint16x8_t a, uint16x8_t b, mve_pred16_t p)
{
  return vbicq_x_u16 (a, b, p);
}

/* { dg-final { scan-assembler "vpst" } } */
/* { dg-final { scan-assembler "vbict"  }  } */

uint16x8_t
foo1 (uint16x8_t a, uint16x8_t b, mve_pred16_t p)
{
  return vbicq_x (a, b, p);
}

/* { dg-final { scan-assembler "vpst" } } */
