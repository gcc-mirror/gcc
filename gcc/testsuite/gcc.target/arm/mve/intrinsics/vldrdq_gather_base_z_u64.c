/* { dg-do compile  } */
/* { dg-additional-options "-march=armv8.1-m.main+mve -mfloat-abi=hard -O2"  }  */
/* { dg-skip-if "Skip if not auto" {*-*-*} {"-mfpu=*"} {"-mfpu=auto"} } */

#include "arm_mve.h"

uint64x2_t
foo (uint64x2_t addr, mve_pred16_t p)
{
  return vldrdq_gather_base_z_u64 (addr, 8, p);
}

/* { dg-final { scan-assembler "vldrdt.u64"  }  } */
