/* { dg-require-effective-target arm_v8_1m_mve_ok } */
/* { dg-add-options arm_v8_1m_mve } */
/* { dg-additional-options "-O2" } */

#include "arm_mve.h"

int32_t
foo (int32_t a, int32x4_t b, int32x4_t c, mve_pred16_t p)
{
  return vmlsdavaxq_p_s32 (a, b, c, p);
}

/* { dg-final { scan-assembler "vmlsdavaxt.s32"  }  } */

int32_t
foo1 (int32_t a, int32x4_t b, int32x4_t c, mve_pred16_t p)
{
  return vmlsdavaxq_p (a, b, c, p);
}

/* { dg-final { scan-assembler "vmlsdavaxt.s32"  }  } */
/* { dg-final { scan-assembler "vmlsdavaxt.s32"  }  } */
