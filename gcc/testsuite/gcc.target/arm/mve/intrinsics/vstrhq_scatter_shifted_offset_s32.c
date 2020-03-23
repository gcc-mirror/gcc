/* { dg-require-effective-target arm_v8_1m_mve_ok } */
/* { dg-add-options arm_v8_1m_mve } */
/* { dg-additional-options "-O2" } */

#include "arm_mve.h"

void
foo (int16_t * base, uint32x4_t offset, int32x4_t value)
{
  vstrhq_scatter_shifted_offset_s32 (base, offset, value);
}

/* { dg-final { scan-assembler "vstrh.32"  }  } */

void
foo1 (int16_t * base, uint32x4_t offset, int32x4_t value)
{
  vstrhq_scatter_shifted_offset (base, offset, value);
}

/* { dg-final { scan-assembler "vstrh.32"  }  } */
