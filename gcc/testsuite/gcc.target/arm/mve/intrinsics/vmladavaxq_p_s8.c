/* { dg-require-effective-target arm_v8_1m_mve_ok } */
/* { dg-add-options arm_v8_1m_mve } */
/* { dg-additional-options "-O2" } */

#include "arm_mve.h"

int32_t
foo (int32_t a, int8x16_t b, int8x16_t c, mve_pred16_t p)
{
  return vmladavaxq_p_s8 (a, b, c, p);
}

/* { dg-final { scan-assembler "vmladavaxt.s8"  }  } */

int32_t
foo1 (int32_t a, int8x16_t b, int8x16_t c, mve_pred16_t p)
{
  return vmladavaxq_p (a, b, c, p);
}

/* { dg-final { scan-assembler "vmladavaxt.s8"  }  } */
/* { dg-final { scan-assembler "vmladavaxt.s8"  }  } */
