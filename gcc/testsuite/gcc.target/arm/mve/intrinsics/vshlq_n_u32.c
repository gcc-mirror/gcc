/* { dg-require-effective-target arm_v8_1m_mve_ok } */
/* { dg-add-options arm_v8_1m_mve } */
/* { dg-additional-options "-O2" } */

#include "arm_mve.h"

uint32x4_t
foo (uint32x4_t a)
{
  return vshlq_n_u32 (a, 1);
}

/* { dg-final { scan-assembler "vshl.u32"  }  } */

uint32x4_t
foo1 (uint32x4_t a)
{
  return vshlq_n (a, 1);
}

/* { dg-final { scan-assembler "vshl.u32"  }  } */
