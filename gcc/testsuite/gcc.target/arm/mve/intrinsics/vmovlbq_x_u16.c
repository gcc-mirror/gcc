/* { dg-require-effective-target arm_v8_1m_mve_ok } */
/* { dg-add-options arm_v8_1m_mve } */
/* { dg-additional-options "-O2" } */

#include "arm_mve.h"

uint32x4_t
foo (uint16x8_t a, mve_pred16_t p)
{
  return vmovlbq_x_u16 (a, p);
}

/* { dg-final { scan-assembler "vpst" } } */
/* { dg-final { scan-assembler "vmovlbt.u16"  }  } */

uint32x4_t
foo1 (uint16x8_t a, mve_pred16_t p)
{
  return vmovlbq_x (a, p);
}

/* { dg-final { scan-assembler "vpst" } } */
