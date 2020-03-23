/* { dg-require-effective-target arm_v8_1m_mve_ok } */
/* { dg-add-options arm_v8_1m_mve } */
/* { dg-additional-options "-O2" } */

#include "arm_mve.h"

uint16x8x4_t
foo (uint16_t const * addr)
{
  return vld4q_u16 (addr);
}

/* { dg-final { scan-assembler "vld40.16"  }  } */
/* { dg-final { scan-assembler "vld41.16"  }  } */
/* { dg-final { scan-assembler "vld42.16"  }  } */
/* { dg-final { scan-assembler "vld43.16"  }  } */

uint16x8x4_t
foo1 (uint16_t const * addr)
{
  return vld4q (addr);
}

/* { dg-final { scan-assembler "vld40.16"  }  } */
