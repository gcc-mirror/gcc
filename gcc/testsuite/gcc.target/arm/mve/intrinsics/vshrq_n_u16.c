/* { dg-require-effective-target arm_v8_1m_mve_ok } */
/* { dg-add-options arm_v8_1m_mve } */
/* { dg-additional-options "-O2" } */

#include "arm_mve.h"

uint16x8_t
foo (uint16x8_t a)
{
  return vshrq_n_u16 (a, 16);
}

/* { dg-final { scan-assembler "vshr.u16"  }  } */

uint16x8_t
foo1 (uint16x8_t a)
{
  return vshrq (a, 16);
}

/* { dg-final { scan-assembler "vshr.u16"  }  } */
