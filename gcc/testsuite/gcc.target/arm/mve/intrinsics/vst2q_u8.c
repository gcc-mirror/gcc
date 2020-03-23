/* { dg-require-effective-target arm_v8_1m_mve_ok } */
/* { dg-add-options arm_v8_1m_mve } */
/* { dg-additional-options "-O2" } */

#include "arm_mve.h"

void
foo (uint8_t * addr, uint8x16x2_t value)
{
  vst2q_u8 (addr, value);
}

/* { dg-final { scan-assembler "vst20.8"  }  } */
/* { dg-final { scan-assembler "vst21.8"  }  } */

void
foo1 (uint8_t * addr, uint8x16x2_t value)
{
  vst2q (addr, value);
}

/* { dg-final { scan-assembler "vst20.8"  }  } */
