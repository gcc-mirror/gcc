/* { dg-require-effective-target arm_v8_1m_mve_fp_ok } */
/* { dg-add-options arm_v8_1m_mve_fp } */
/* { dg-additional-options "-O2" } */

#include "arm_mve.h"

int32_t
foo (int8x16_t a)
{
  return vaddvq_s8 (a);
}

/* { dg-final { scan-assembler "vaddv.s8"  }  } */

int32_t
foo1 (int8x16_t a)
{
  return vaddvq (a);
}

/* { dg-final { scan-assembler "vaddv.s8"  }  } */
