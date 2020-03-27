/* { dg-require-effective-target arm_v8_1m_mve_fp_ok } */
/* { dg-add-options arm_v8_1m_mve_fp } */
/* { dg-additional-options "-O2" } */

#include "arm_mve.h"

float32x4x4_t
foo (float32_t const * addr)
{
  return vld4q_f32 (addr);
}

/* { dg-final { scan-assembler "vld40.32"  }  } */
/* { dg-final { scan-assembler "vld41.32"  }  } */
/* { dg-final { scan-assembler "vld42.32"  }  } */
/* { dg-final { scan-assembler "vld43.32"  }  } */

float32x4x4_t
foo1 (float32_t const * addr)
{
  return vld4q (addr);
}

/* { dg-final { scan-assembler "vld40.32"  }  } */
