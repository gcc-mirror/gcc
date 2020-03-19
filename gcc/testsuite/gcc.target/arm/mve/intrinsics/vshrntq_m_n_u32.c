/* { dg-do compile  } */
/* { dg-additional-options "-march=armv8.1-m.main+mve -mfloat-abi=hard -O2"  }  */
/* { dg-skip-if "Skip if not auto" {*-*-*} {"-mfpu=*"} {"-mfpu=auto"} } */

#include "arm_mve.h"

uint16x8_t
foo (uint16x8_t a, uint32x4_t b, mve_pred16_t p)
{
  return vshrntq_m_n_u32 (a, b, 16, p);
}

/* { dg-final { scan-assembler "vpst" } } */
/* { dg-final { scan-assembler "vshrntt.i32"  }  } */

uint16x8_t
foo1 (uint16x8_t a, uint32x4_t b, mve_pred16_t p)
{
  return vshrntq_m (a, b, 16, p);
}

/* { dg-final { scan-assembler "vpst" } } */
/* { dg-final { scan-assembler "vshrntt.i32"  }  } */
