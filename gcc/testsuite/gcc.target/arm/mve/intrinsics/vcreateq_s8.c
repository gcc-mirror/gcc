/* { dg-require-effective-target arm_v8_1m_mve_ok } */
/* { dg-add-options arm_v8_1m_mve } */
/* { dg-additional-options "-O2" } */

#include "arm_mve.h"

int8x16_t
foo (uint64_t a, uint64_t b)
{
  return vcreateq_s8 (a, b);
}

/* { dg-final { scan-assembler "vmov"  }  } */
