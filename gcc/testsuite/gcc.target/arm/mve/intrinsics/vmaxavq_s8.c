/* { dg-require-effective-target arm_v8_1m_mve_ok } */
/* { dg-add-options arm_v8_1m_mve } */
/* { dg-additional-options "-O2" } */

#include "arm_mve.h"

uint8_t
foo (uint8_t a, int8x16_t b)
{
  return vmaxavq_s8 (a, b);
}

/* { dg-final { scan-assembler "vmaxav.s8"  }  } */

uint8_t
foo1 (uint8_t a, int8x16_t b)
{
  return vmaxavq (a, b);
}

/* { dg-final { scan-assembler "vmaxav.s8"  }  } */
