/* { dg-require-effective-target arm_v8_1m_mve_ok } */
/* { dg-add-options arm_v8_1m_mve } */
/* { dg-additional-options "-O2" } */

#include "arm_mve.h"

void
foo (int8_t * base, uint8x16_t offset, int8x16_t value, mve_pred16_t p)
{
  vstrbq_scatter_offset_p_s8 (base, offset, value, p);
}

/* { dg-final { scan-assembler "vstrbt.8"  }  } */

void
foo1 (int8_t * base, uint8x16_t offset, int8x16_t value, mve_pred16_t p)
{
  vstrbq_scatter_offset_p (base, offset, value, p);
}

/* { dg-final { scan-assembler "vstrbt.8"  }  } */
