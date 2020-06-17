/* { dg-require-effective-target arm_v8_1m_mve_ok } */
/* { dg-add-options arm_v8_1m_mve } */
/* { dg-additional-options "-O2" } */

#include "arm_mve.h"

int16x8_t
foo (int8x16_t a)
{
  return vmovlbq_s8 (a);
}

/* { dg-final { scan-assembler "vmovlb.s8"  }  } */

int16x8_t
foo1 (int8x16_t a)
{
  return vmovlbq (a);
}

/* { dg-final { scan-assembler "vmovlb.s8"  }  } */
