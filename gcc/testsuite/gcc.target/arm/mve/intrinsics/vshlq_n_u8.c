/* { dg-require-effective-target arm_v8_1m_mve_ok } */
/* { dg-add-options arm_v8_1m_mve } */
/* { dg-additional-options "-O2" } */

#include "arm_mve.h"

uint8x16_t
foo (uint8x16_t a)
{
  return vshlq_n_u8 (a, 1);
}

/* { dg-final { scan-assembler "vshl.u8"  }  } */

uint8x16_t
foo1 (uint8x16_t a)
{
  return vshlq_n (a, 1);
}

/* { dg-final { scan-assembler "vshl.u8"  }  } */
