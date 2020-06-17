/* { dg-require-effective-target arm_v8_1m_mve_fp_ok } */
/* { dg-add-options arm_v8_1m_mve_fp } */
/* { dg-additional-options "-O2" } */

#include "arm_mve.h"

void
foo (float16_t * addr, float16x8x2_t value)
{
  vst2q_f16 (addr, value);
}

/* { dg-final { scan-assembler "vst20.16"  }  } */
/* { dg-final { scan-assembler "vst21.16"  }  } */

void
foo1 (float16_t * addr, float16x8x2_t value)
{
  vst2q (addr, value);
}

/* { dg-final { scan-assembler "vst20.16"  }  } */
