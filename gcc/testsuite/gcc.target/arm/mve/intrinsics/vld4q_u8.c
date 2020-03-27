/* { dg-require-effective-target arm_v8_1m_mve_ok } */
/* { dg-add-options arm_v8_1m_mve } */
/* { dg-additional-options "-O2" } */

#include "arm_mve.h"

uint8x16x4_t
foo (uint8_t const * addr)
{
  return vld4q_u8 (addr);
}

/* { dg-final { scan-assembler "vld40.8"  }  } */
/* { dg-final { scan-assembler "vld41.8"  }  } */
/* { dg-final { scan-assembler "vld42.8"  }  } */
/* { dg-final { scan-assembler "vld43.8"  }  } */

uint8x16x4_t
foo1 (uint8_t const * addr)
{
  return vld4q (addr);
}

/* { dg-final { scan-assembler "vld40.8"  }  } */
