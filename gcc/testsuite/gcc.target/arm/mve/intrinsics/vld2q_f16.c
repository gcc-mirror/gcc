/* { dg-require-effective-target arm_v8_1m_mve_fp_ok } */
/* { dg-add-options arm_v8_1m_mve_fp } */
/* { dg-additional-options "-O2" } */

#include "arm_mve.h"

float16x8x2_t
foo (float16_t const * addr)
{
  return vld2q_f16 (addr);
}

/* { dg-final { scan-assembler "vld20.16"  }  } */
/* { dg-final { scan-assembler "vld21.16"  }  } */

float16x8x2_t
foo1 (float16_t const * addr)
{
  return vld2q (addr);
}

/* { dg-final { scan-assembler "vld20.16"  }  } */
