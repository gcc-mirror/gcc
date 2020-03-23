/* { dg-require-effective-target arm_v8_1m_mve_ok } */
/* { dg-add-options arm_v8_1m_mve } */
/* { dg-additional-options "-O2" } */

#include "arm_mve.h"

void
foo (int16_t * addr, int16x8x4_t value)
{
  vst4q_s16 (addr, value);
}

/* { dg-final { scan-assembler "vst40.16"  }  } */
/* { dg-final { scan-assembler "vst41.16"  }  } */
/* { dg-final { scan-assembler "vst42.16"  }  } */
/* { dg-final { scan-assembler "vst43.16"  }  } */

void
foo1 (int16_t * addr, int16x8x4_t value)
{
  vst4q (addr, value);
}

/* { dg-final { scan-assembler "vst40.16"  }  } */
/* { dg-final { scan-assembler "vst41.16"  }  } */
/* { dg-final { scan-assembler "vst42.16"  }  } */
/* { dg-final { scan-assembler "vst43.16"  }  } */

void
foo2 (int16_t * addr, int16x8x4_t value)
{
  vst4q_s16 (addr, value);
  addr += 32;
  vst4q_s16 (addr, value);
}

/* { dg-final { scan-assembler {vst43.16\s\{.*\}, \[.*\]!}  }  } */
