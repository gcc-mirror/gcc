/* { dg-require-effective-target arm_v8_1m_mve_ok } */
/* { dg-add-options arm_v8_1m_mve } */
/* { dg-additional-options "-O2" } */

#include "arm_mve.h"

void
foo (uint8_t * addr, uint16x8_t value)
{
  vstrbq_u16 (addr, value);
}

/* { dg-final { scan-assembler "vstrb.16"  }  } */

void
foo1 (uint8_t * addr, uint16x8_t value)
{
  vstrbq (addr, value);
}

/* { dg-final { scan-assembler "vstrb.16"  }  } */
