/* { dg-require-effective-target arm_v8_1m_mve_ok } */
/* { dg-add-options arm_v8_1m_mve } */
/* { dg-additional-options "-O2" } */

#include "arm_mve.h"

int32x4x4_t
foo (int32_t const * addr)
{
  return vld4q_s32 (addr);
}

/* { dg-final { scan-assembler "vld40.32"  }  } */
/* { dg-final { scan-assembler "vld41.32"  }  } */
/* { dg-final { scan-assembler "vld42.32"  }  } */
/* { dg-final { scan-assembler "vld43.32"  }  } */

int32x4x4_t
foo1 (int32_t const * addr)
{
  return vld4q (addr);
}

/* { dg-final { scan-assembler "vld40.32"  }  } */
