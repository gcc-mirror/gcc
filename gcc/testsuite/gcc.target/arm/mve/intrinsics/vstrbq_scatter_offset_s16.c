/* { dg-require-effective-target arm_v8_1m_mve_ok } */
/* { dg-add-options arm_v8_1m_mve } */
/* { dg-additional-options "-O2" } */

#include "arm_mve.h"

void
foo (int8_t * base, uint16x8_t offset, int16x8_t value)
{
  vstrbq_scatter_offset_s16 (base, offset, value);
}

/* { dg-final { scan-assembler "vstrb.16"  }  } */

void
foo1 (int8_t * base, uint16x8_t offset, int16x8_t value)
{
  vstrbq_scatter_offset (base, offset, value);
}

/* { dg-final { scan-assembler "vstrb.16"  }  } */
