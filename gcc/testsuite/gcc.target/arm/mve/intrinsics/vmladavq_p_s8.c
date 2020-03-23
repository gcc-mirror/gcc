/* { dg-require-effective-target arm_v8_1m_mve_ok } */
/* { dg-add-options arm_v8_1m_mve } */
/* { dg-additional-options "-O2" } */

#include "arm_mve.h"

int32_t
foo (int8x16_t a, int8x16_t b, mve_pred16_t p)
{
  return vmladavq_p_s8 (a, b, p);
}

/* { dg-final { scan-assembler "vmladavt.s8"  }  } */

int32_t
foo1 (int8x16_t a, int8x16_t b, mve_pred16_t p)
{
  return vmladavq_p (a, b, p);
}

/* { dg-final { scan-assembler "vmladavt.s8"  }  } */
