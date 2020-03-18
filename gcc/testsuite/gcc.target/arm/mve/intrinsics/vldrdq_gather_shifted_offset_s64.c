/* { dg-do compile  } */
/* { dg-additional-options "-march=armv8.1-m.main+mve -mfloat-abi=hard -O2"  }  */
/* { dg-skip-if "Skip if not auto" {*-*-*} {"-mfpu=*"} {"-mfpu=auto"} } */

#include "arm_mve.h"

int64x2_t
foo (int64_t const * base, uint64x2_t offset)
{
  return vldrdq_gather_shifted_offset_s64 (base, offset);
}

/* { dg-final { scan-assembler "vldrd.u64"  }  } */

int64x2_t
foo1 (int64_t const * base, uint64x2_t offset)
{
  return vldrdq_gather_shifted_offset (base, offset);
}

/* { dg-final { scan-assembler "vldrd.u64"  }  } */
