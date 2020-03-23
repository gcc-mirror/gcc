/* { dg-require-effective-target arm_v8_1m_mve_ok } */
/* { dg-add-options arm_v8_1m_mve } */
/* { dg-additional-options "-O2" } */

#include "arm_mve.h"

uint64x2_t
foo (uint32x4_t a, uint32x4_t b)
{
  return vmulltq_int_u32 (a, b);
}

/* { dg-final { scan-assembler "vmullt.u32"  }  } */

uint64x2_t
foo1 (uint32x4_t a, uint32x4_t b)
{
  return vmulltq_int (a, b);
}

/* { dg-final { scan-assembler "vmullt.u32"  }  } */
