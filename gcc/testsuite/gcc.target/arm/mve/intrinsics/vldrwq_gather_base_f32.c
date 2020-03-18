/* { dg-do compile  } */
/* { dg-additional-options "-march=armv8.1-m.main+mve.fp -mfloat-abi=hard -O2"  }  */
/* { dg-skip-if "Skip if not auto" {*-*-*} {"-mfpu=*"} {"-mfpu=auto"} } */

#include "arm_mve.h"

float32x4_t
foo (uint32x4_t addr)
{
  return vldrwq_gather_base_f32 (addr, 4);
}

/* { dg-final { scan-assembler "vldrw.u32"  }  } */
