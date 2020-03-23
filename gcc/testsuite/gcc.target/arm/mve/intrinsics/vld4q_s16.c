/* { dg-require-effective-target arm_v8_1m_mve_ok } */
/* { dg-add-options arm_v8_1m_mve } */
/* { dg-additional-options "-O2" } */

#include "arm_mve.h"

int16x8x4_t
foo (int16_t const * addr)
{
  return vld4q_s16 (addr);
}

/* { dg-final { scan-assembler "vld40.16"  }  } */
/* { dg-final { scan-assembler "vld41.16"  }  } */
/* { dg-final { scan-assembler "vld42.16"  }  } */
/* { dg-final { scan-assembler "vld43.16"  }  } */

int16x8x4_t
foo1 (int16_t const * addr)
{
  return vld4q (addr);
}

/* { dg-final { scan-assembler "vld40.16"  }  } */
