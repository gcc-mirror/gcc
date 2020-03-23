/* { dg-require-effective-target arm_v8_1m_mve_ok } */
/* { dg-add-options arm_v8_1m_mve } */
/* { dg-additional-options "-O2" } */

#include "arm_mve.h"

void
foo (uint8_t * base, uint16x8_t offset, uint16x8_t value)
{
  vstrbq_scatter_offset_u16 (base, offset, value);
}

/* { dg-final { scan-assembler "vstrb.16"  }  } */

void
foo1 (uint8_t * base, uint16x8_t offset, uint16x8_t value)
{
  vstrbq_scatter_offset (base, offset, value);
}

/* { dg-final { scan-assembler "vstrb.16"  }  } */
