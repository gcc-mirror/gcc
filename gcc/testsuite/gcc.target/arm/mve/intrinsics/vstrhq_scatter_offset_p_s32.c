/* { dg-require-effective-target arm_v8_1m_mve_ok } */
/* { dg-add-options arm_v8_1m_mve } */
/* { dg-additional-options "-O2" } */

#include "arm_mve.h"

void
foo (int16_t * base, uint32x4_t offset, int32x4_t value, mve_pred16_t p)
{
  vstrhq_scatter_offset_p_s32 (base, offset, value, p);
}

/* { dg-final { scan-assembler "vstrht.32"  }  } */

void
foo1 (int16_t * base, uint32x4_t offset, int32x4_t value, mve_pred16_t p)
{
  vstrhq_scatter_offset_p (base, offset, value, p);
}

/* { dg-final { scan-assembler "vstrht.32"  }  } */
