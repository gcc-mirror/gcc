/* { dg-require-effective-target arm_v8_1m_mve_fp_ok } */
/* { dg-add-options arm_v8_1m_mve_fp } */
/* { dg-additional-options "-O2" } */

#include "arm_mve.h"

float16x8x4_t
foo (float16_t const * addr)
{
  return vld4q_f16 (addr);
}

/* { dg-final { scan-assembler "vld40.16"  }  } */
/* { dg-final { scan-assembler "vld41.16"  }  } */
/* { dg-final { scan-assembler "vld42.16"  }  } */
/* { dg-final { scan-assembler "vld43.16"  }  } */

float16x8x4_t
foo1 (float16_t const * addr)
{
  return vld4q (addr);
}

/* { dg-final { scan-assembler "vld40.16"  }  } */
