/* { dg-require-effective-target arm_v8_1m_mve_ok } */
/* { dg-add-options arm_v8_1m_mve } */
/* { dg-additional-options "-O2" } */

#include "arm_mve.h"

int64x2_t
foo (int64x2_t a, int64x2_t b, mve_pred16_t p)
{
  return vpselq_s64 (a, b, p);
}

/* { dg-final { scan-assembler "vpsel"  }  } */

int64x2_t
foo1 (int64x2_t a, int64x2_t b, mve_pred16_t p)
{
  return vpselq (a, b, p);
}

/* { dg-final { scan-assembler "vpsel"  }  } */
