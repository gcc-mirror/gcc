/* { dg-require-effective-target arm_v8_1m_mve_fp_ok } */
/* { dg-add-options arm_v8_1m_mve_fp } */
/* { dg-additional-options "-O2" } */

#include "arm_mve.h"

void
foo (float32_t * addr, float32x4_t value, mve_pred16_t p)
{
  vst1q_p_f32 (addr, value, p);
}

/* { dg-final { scan-assembler "vstrwt.32"  }  } */

void
foo1 (float32_t * addr, float32x4_t value, mve_pred16_t p)
{
  vst1q_p (addr, value, p);
}

/* { dg-final { scan-assembler "vstrwt.32"  }  } */
