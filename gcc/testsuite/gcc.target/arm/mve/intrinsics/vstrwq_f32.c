/* { dg-require-effective-target arm_v8_1m_mve_fp_ok } */
/* { dg-add-options arm_v8_1m_mve_fp } */
/* { dg-additional-options "-O2" } */

#include "arm_mve.h"

void
foo (float32_t * addr, float32x4_t value)
{
  vstrwq_f32 (addr, value);
}

/* { dg-final { scan-assembler "vstrw.32"  }  } */

void
foo1 (float32_t * addr, float32x4_t value)
{
  vstrwq (addr, value);
}

/* { dg-final { scan-assembler "vstrw.32"  }  } */
