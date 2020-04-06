/* { dg-require-effective-target arm_v8_1m_mve_ok } */
/* { dg-add-options arm_v8_1m_mve } */
/* { dg-additional-options "-O2" } */

#include "arm_mve.h"

uint32x4x4_t
foo (uint32_t const * addr)
{
  return vld4q_u32 (addr);
}

/* { dg-final { scan-assembler "vld40.32"  }  } */
/* { dg-final { scan-assembler "vld41.32"  }  } */
/* { dg-final { scan-assembler "vld42.32"  }  } */
/* { dg-final { scan-assembler "vld43.32"  }  } */

uint32x4x4_t
foo1 (uint32_t const * addr)
{
  return vld4q (addr);
}

/* { dg-final { scan-assembler "vld40.32"  }  } */
