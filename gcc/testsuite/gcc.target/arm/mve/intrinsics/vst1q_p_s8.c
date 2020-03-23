/* { dg-require-effective-target arm_v8_1m_mve_ok } */
/* { dg-add-options arm_v8_1m_mve } */
/* { dg-additional-options "-O2" } */

#include "arm_mve.h"

void
foo (int8_t * addr, int8x16_t value, mve_pred16_t p)
{
  vst1q_p_s8 (addr, value, p);
}

/* { dg-final { scan-assembler "vstrbt.8"  }  } */

void
foo1 (int8_t * addr, int8x16_t value, mve_pred16_t p)
{
  vst1q_p (addr, value, p);
}

/* { dg-final { scan-assembler "vstrbt.8"  }  } */
