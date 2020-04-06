/* { dg-require-effective-target arm_v8_1m_mve_ok } */
/* { dg-add-options arm_v8_1m_mve } */
/* { dg-additional-options "-O2" } */

#include "arm_mve.h"

void
foo (int8_t * addr, int16x8_t value, mve_pred16_t p)
{
  vstrbq_p_s16 (addr, value, p);
}

/* { dg-final { scan-assembler "vstrbt.16"  }  } */

void
foo1 (int8_t * addr, int16x8_t value, mve_pred16_t p)
{
  vstrbq_p (addr, value, p);
}

/* { dg-final { scan-assembler "vstrbt.16"  }  } */
