/* { dg-do compile  } */
/* { dg-additional-options "-march=armv8.1-m.main+mve.fp -mfloat-abi=hard -O2"  }  */
/* { dg-skip-if "Skip if not auto" {*-*-*} {"-mfpu=*"} {"-mfpu=auto"} } */

#include "arm_mve.h"

float16x8_t
foo (float16_t const * base, uint16x8_t offset, mve_pred16_t p)
{
  return vldrhq_gather_shifted_offset_z_f16 (base, offset, p);
}

/* { dg-final { scan-assembler "vldrht.f16"  }  } */

float16x8_t
foo1 (float16_t const * base, uint16x8_t offset, mve_pred16_t p)
{
  return vldrhq_gather_shifted_offset_z (base, offset, p);
}

/* { dg-final { scan-assembler "vldrht.f16"  }  } */
