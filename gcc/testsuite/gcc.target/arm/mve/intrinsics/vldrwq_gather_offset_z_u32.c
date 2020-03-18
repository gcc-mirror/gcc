/* { dg-do compile  } */
/* { dg-additional-options "-march=armv8.1-m.main+mve -mfloat-abi=hard -O2"  }  */
/* { dg-skip-if "Skip if not auto" {*-*-*} {"-mfpu=*"} {"-mfpu=auto"} } */

#include "arm_mve.h"

uint32x4_t
foo (uint32_t const * base, uint32x4_t offset, mve_pred16_t p)
{
  return vldrwq_gather_offset_z_u32 (base, offset, p);
}

/* { dg-final { scan-assembler "vldrwt.u32"  }  } */

uint32x4_t
foo1 (uint32_t const * base, uint32x4_t offset, mve_pred16_t p)
{
  return vldrwq_gather_offset_z (base, offset, p);
}

/* { dg-final { scan-assembler "vldrwt.u32"  }  } */
