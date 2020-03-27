/* { dg-require-effective-target arm_v8_1m_mve_ok } */
/* { dg-add-options arm_v8_1m_mve } */
/* { dg-additional-options "-O2" } */

#include "arm_mve.h"

uint16_t
foo (uint16_t a, int16x8_t b)
{
  return vminavq_s16 (a, b);
}

/* { dg-final { scan-assembler "vminav.s16"  }  } */

uint16_t
foo1 (uint16_t a, int16x8_t b)
{
  return vminavq (a, b);
}

/* { dg-final { scan-assembler "vminav.s16"  }  } */
