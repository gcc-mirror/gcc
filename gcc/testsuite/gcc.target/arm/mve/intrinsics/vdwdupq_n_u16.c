/* { dg-require-effective-target arm_v8_1m_mve_ok } */
/* { dg-add-options arm_v8_1m_mve } */
/* { dg-additional-options "-O2" } */

#include "arm_mve.h"

uint16x8_t
foo (uint32_t a, uint32_t b)
{
  return vdwdupq_n_u16 (a, b, 2);
}

/* { dg-final { scan-assembler "vdwdup.u16"  }  } */

uint16x8_t
foo1 (uint32_t a, uint32_t b)
{
  return vdwdupq_u16 (a, b, 2);
}

/* { dg-final { scan-assembler "vdwdup.u16"  }  } */
