/* { dg-do compile  } */
/* { dg-additional-options "-march=armv8.1-m.main+mve.fp -mfloat-abi=hard -O2"  }  */
/* { dg-skip-if "Skip if not auto" {*-*-*} {"-mfpu=*"} {"-mfpu=auto"} } */

#include "arm_mve.h"

float32x4_t
foo (float32_t const * base, uint32x4_t offset)
{
  return vldrwq_gather_offset_f32 (base, offset);
}

/* { dg-final { scan-assembler "vldrw.u32"  }  } */

float32x4_t
foo1 (float32_t const * base, uint32x4_t offset)
{
  return vldrwq_gather_offset (base, offset);
}

/* { dg-final { scan-assembler "vldrw.u32"  }  } */
