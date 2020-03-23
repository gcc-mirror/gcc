/* { dg-require-effective-target arm_v8_1m_mve_fp_ok } */
/* { dg-add-options arm_v8_1m_mve_fp } */
/* { dg-additional-options "-O2" } */

#include "arm_mve.h"

void
foo (float16_t * base, uint16x8_t offset, float16x8_t value, mve_pred16_t p)
{
  vstrhq_scatter_offset_p_f16 (base, offset, value, p);
}

/* { dg-final { scan-assembler "vstrht.16"  }  } */

void
foo1 (float16_t * base, uint16x8_t offset, float16x8_t value, mve_pred16_t p)
{
  vstrhq_scatter_offset_p (base, offset, value, p);
}

/* { dg-final { scan-assembler "vstrht.16"  }  } */
