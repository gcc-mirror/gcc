/* { dg-require-effective-target arm_v8_1m_mve_ok } */
/* { dg-add-options arm_v8_1m_mve } */
/* { dg-additional-options "-O2" } */

#include "arm_mve.h"

void
foo (int8_t * addr, int8x16x4_t value)
{
  vst4q_s8 (addr, value);
}

/* { dg-final { scan-assembler "vst40.8"  }  } */
/* { dg-final { scan-assembler "vst41.8"  }  } */
/* { dg-final { scan-assembler "vst42.8"  }  } */
/* { dg-final { scan-assembler "vst43.8"  }  } */

void
foo1 (int8_t * addr, int8x16x4_t value)
{
  vst4q (addr, value);
}

/* { dg-final { scan-assembler "vst40.8"  }  } */
/* { dg-final { scan-assembler "vst41.8"  }  } */
/* { dg-final { scan-assembler "vst42.8"  }  } */
/* { dg-final { scan-assembler "vst43.8"  }  } */

void
foo2 (int8_t * addr, int8x16x4_t value)
{
  vst4q_s8 (addr, value);
  addr += 16*4;
  vst4q_s8 (addr, value);
}

/* { dg-final { scan-assembler {vst43.8\s\{.*\}, \[.*\]!}  }  } */
