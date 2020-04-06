/* { dg-require-effective-target arm_v8_1m_mve_fp_ok } */
/* { dg-add-options arm_v8_1m_mve_fp } */
/* { dg-additional-options "-O2" } */

#include "arm_mve.h"

void
foo (float32_t * base, uint32x4_t offset, float32x4_t value, mve_pred16_t p)
{
  vstrwq_scatter_offset_p_f32 (base, offset, value, p);
}

/* { dg-final { scan-assembler "vstrwt.32"  }  } */

void
foo1 (float32_t * base, uint32x4_t offset, float32x4_t value, mve_pred16_t p)
{
  vstrwq_scatter_offset_p (base, offset, value, p);
}

/* { dg-final { scan-assembler "vstrwt.32"  }  } */
