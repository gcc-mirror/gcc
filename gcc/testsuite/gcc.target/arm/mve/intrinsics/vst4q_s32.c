/* { dg-require-effective-target arm_v8_1m_mve_ok } */
/* { dg-add-options arm_v8_1m_mve } */
/* { dg-additional-options "-O2" } */

#include "arm_mve.h"

void
foo (int32_t * addr, int32x4x4_t value)
{
  vst4q_s32 (addr, value);
}

/* { dg-final { scan-assembler "vst40.32"  }  } */
/* { dg-final { scan-assembler "vst41.32"  }  } */
/* { dg-final { scan-assembler "vst42.32"  }  } */
/* { dg-final { scan-assembler "vst43.32"  }  } */

void
foo1 (int32_t * addr, int32x4x4_t value)
{
  vst4q (addr, value);
}

/* { dg-final { scan-assembler "vst40.32"  }  } */
/* { dg-final { scan-assembler "vst41.32"  }  } */
/* { dg-final { scan-assembler "vst42.32"  }  } */
/* { dg-final { scan-assembler "vst43.32"  }  } */

void
foo2 (int32_t * addr, int32x4x4_t value)
{
  vst4q_s32 (addr, value);
  addr += 16;
  vst4q_s32 (addr, value); 
}

/* { dg-final { scan-assembler {vst43.32\s\{.*\}, \[.*\]!}  }  } */
