/* { dg-do compile  } */
/* { dg-additional-options "-march=armv8.1-m.main+mve -mfloat-abi=hard -O2"  }  */
/* { dg-skip-if "Skip if not auto" {*-*-*} {"-mfpu=*"} {"-mfpu=auto"} } */

#include "arm_mve.h"

int64x2_t
foo (int64_t const * base, uint64x2_t offset, mve_pred16_t p)
{
  return vldrdq_gather_shifted_offset_z_s64 (base, offset, p);
}

/* { dg-final { scan-assembler "vldrdt.u64"  }  } */

int64x2_t
foo1 (int64_t const * base, uint64x2_t offset, mve_pred16_t p)
{
  return vldrdq_gather_shifted_offset_z (base, offset, p);
}

/* { dg-final { scan-assembler "vldrdt.u64"  }  } */
