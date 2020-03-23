/* { dg-require-effective-target arm_v8_1m_mve_ok } */
/* { dg-add-options arm_v8_1m_mve } */
/* { dg-additional-options "-O2" } */

#include "arm_mve.h"

uint16x8_t
foo (uint16x8_t a, int32_t b)
{
  return vbrsrq_n_u16 (a, b);
}

/* { dg-final { scan-assembler "vbrsr.16"  }  } */

uint16x8_t
foo1 (uint16x8_t a, int32_t b)
{
  return vbrsrq (a, b);
}

/* { dg-final { scan-assembler "vbrsr.16"  }  } */
