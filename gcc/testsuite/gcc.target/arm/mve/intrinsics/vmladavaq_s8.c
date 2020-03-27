/* { dg-require-effective-target arm_v8_1m_mve_ok } */
/* { dg-add-options arm_v8_1m_mve } */
/* { dg-additional-options "-O2" } */

#include "arm_mve.h"

int32_t
foo (int32_t a, int8x16_t b, int8x16_t c)
{
  return vmladavaq_s8 (a, b, c);
}

/* { dg-final { scan-assembler "vmladava.s8"  }  } */

int32_t
foo1 (int32_t a, int8x16_t b, int8x16_t c)
{
  return vmladavaq (a, b, c);
}

/* { dg-final { scan-assembler "vmladava.s8"  }  } */
