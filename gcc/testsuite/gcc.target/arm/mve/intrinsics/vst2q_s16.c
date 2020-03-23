/* { dg-require-effective-target arm_v8_1m_mve_ok } */
/* { dg-add-options arm_v8_1m_mve } */
/* { dg-additional-options "-O2" } */

#include "arm_mve.h"

void
foo (int16_t * addr, int16x8x2_t value)
{
  vst2q_s16 (addr, value);
}

/* { dg-final { scan-assembler "vst20.16"  }  } */
/* { dg-final { scan-assembler "vst21.16"  }  } */

void
foo1 (int16_t * addr, int16x8x2_t value)
{
  vst2q (addr, value);
}

/* { dg-final { scan-assembler "vst20.16"  }  } */
