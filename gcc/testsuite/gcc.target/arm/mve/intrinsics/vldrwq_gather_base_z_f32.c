/* { dg-do compile  } */
/* { dg-additional-options "-march=armv8.1-m.main+mve.fp -mfloat-abi=hard -O2"  }  */
/* { dg-skip-if "Skip if not auto" {*-*-*} {"-mfpu=*"} {"-mfpu=auto"} } */

#include "arm_mve.h"

float32x4_t
foo (uint32x4_t addr, mve_pred16_t p)
{
  return vldrwq_gather_base_z_f32 (addr, 4, p);
}

/* { dg-final { scan-assembler "vldrwt.u32"  }  } */
