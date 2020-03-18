/* { dg-do compile  } */
/* { dg-additional-options "-march=armv8.1-m.main+mve.fp -mfloat-abi=hard -O2"  }  */
/* { dg-skip-if "Skip if not auto" {*-*-*} {"-mfpu=*"} {"-mfpu=auto"} } */

#include "arm_mve.h"

float32x4_t
foo (float32_t const * base, uint32x4_t offset, mve_pred16_t p)
{
  return vldrwq_gather_shifted_offset_z_f32 (base, offset, p);
}

/* { dg-final { scan-assembler "vldrwt.u32"  }  } */

float32x4_t
foo1 (float32_t const * base, uint32x4_t offset, mve_pred16_t p)
{
  return vldrwq_gather_shifted_offset_z (base, offset, p);
}

/* { dg-final { scan-assembler "vldrwt.u32"  }  } */
