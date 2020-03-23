/* { dg-require-effective-target arm_v8_1m_mve_ok } */
/* { dg-add-options arm_v8_1m_mve } */
/* { dg-additional-options "-O2" } */

#include "arm_mve.h"

uint32x4_t
foo (uint32x4_t a, uint32x4_t b, uint32_t c)
{
  return vqdmlahq_n_u32 (a, b, c);
}

/* { dg-final { scan-assembler "vqdmlah.s32"  }  } */

uint32x4_t
foo1 (uint32x4_t a, uint32x4_t b, uint32_t c)
{
  return vqdmlahq (a, b, c);
}

/* { dg-final { scan-assembler "vqdmlah.s32"  }  } */
