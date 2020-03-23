/* { dg-require-effective-target arm_v8_1m_mve_ok } */
/* { dg-add-options arm_v8_1m_mve } */
/* { dg-additional-options "-O2" } */

#include "arm_mve.h"

uint32x4_t
foo (uint32x4_t a)
{
  return vorrq_n_u32 (a, 44);
}

/* { dg-final { scan-assembler "vorr.i32"  }  } */
