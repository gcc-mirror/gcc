/* { dg-require-effective-target arm_v8_1m_mve_ok } */
/* { dg-add-options arm_v8_1m_mve } */
/* { dg-additional-options "-O2" } */

#include "arm_mve.h"

uint8x16_t
foo (int8x16_t a)
{
  return vqshluq_n_s8 (a, 7);
}

/* { dg-final { scan-assembler "vqshlu.s8"  }  } */

uint8x16_t
foo1 (int8x16_t a)
{
  return vqshluq (a, 7);
}

/* { dg-final { scan-assembler "vqshlu.s8"  }  } */
