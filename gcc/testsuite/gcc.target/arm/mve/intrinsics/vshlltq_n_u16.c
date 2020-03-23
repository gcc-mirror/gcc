/* { dg-require-effective-target arm_v8_1m_mve_ok } */
/* { dg-add-options arm_v8_1m_mve } */
/* { dg-additional-options "-O2" } */

#include "arm_mve.h"

uint32x4_t
foo (uint16x8_t a)
{
  return vshlltq_n_u16 (a, 1);
}

/* { dg-final { scan-assembler "vshllt.u16"  }  } */

uint32x4_t
foo1 (uint16x8_t a)
{
  return vshlltq (a, 1);
}

/* { dg-final { scan-assembler "vshllt.u16"  }  } */
