/* { dg-require-effective-target arm_v8_1m_mve_fp_ok } */
/* { dg-add-options arm_v8_1m_mve_fp } */
/* { dg-additional-options "-O2" } */

#include "arm_mve.h"

void
foo (float32_t * addr, float32x4x4_t value)
{
  vst4q_f32 (addr, value);
}

/* { dg-final { scan-assembler "vst40.32"  }  } */
/* { dg-final { scan-assembler "vst41.32"  }  } */
/* { dg-final { scan-assembler "vst42.32"  }  } */
/* { dg-final { scan-assembler "vst43.32"  }  } */

void
foo1 (float32_t * addr, float32x4x4_t value)
{
  vst4q (addr, value);
}

/* { dg-final { scan-assembler "vst40.32"  }  } */
/* { dg-final { scan-assembler "vst41.32"  }  } */
/* { dg-final { scan-assembler "vst42.32"  }  } */
/* { dg-final { scan-assembler "vst43.32"  }  } */

void
foo2 (float32_t * addr, float32x4x4_t value)
{
  vst4q_f32 (addr, value);
  addr += 16;
  vst4q_f32 (addr, value);
}

/* { dg-final { scan-assembler {vst43.32\s\{.*\}, \[.*\]!}  }  } */
