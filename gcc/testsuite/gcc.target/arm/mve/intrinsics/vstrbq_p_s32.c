/* { dg-require-effective-target arm_v8_1m_mve_ok } */
/* { dg-add-options arm_v8_1m_mve } */
/* { dg-additional-options "-O2" } */

#include "arm_mve.h"

void
foo (int8_t * addr, int32x4_t value, mve_pred16_t p)
{
  vstrbq_p_s32 (addr, value, p);
}

/* { dg-final { scan-assembler "vstrbt.32"  }  } */

void
foo1 (int8_t * addr, int32x4_t value, mve_pred16_t p)
{
  vstrbq_p (addr, value, p);
}

/* { dg-final { scan-assembler "vstrbt.32"  }  } */
