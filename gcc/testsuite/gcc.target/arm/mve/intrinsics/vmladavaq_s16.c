/* { dg-require-effective-target arm_v8_1m_mve_ok } */
/* { dg-add-options arm_v8_1m_mve } */
/* { dg-additional-options "-O2" } */

#include "arm_mve.h"

int32_t
foo (int32_t a, int16x8_t b, int16x8_t c)
{
  return vmladavaq_s16 (a, b, c);
}

/* { dg-final { scan-assembler "vmladava.s16"  }  } */

int32_t
foo1 (int32_t a, int16x8_t b, int16x8_t c)
{
  return vmladavaq (a, b, c);
}

/* { dg-final { scan-assembler "vmladava.s16"  }  } */
