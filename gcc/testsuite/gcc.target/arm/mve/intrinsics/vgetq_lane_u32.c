/* { dg-skip-if "Incompatible float ABI" { *-*-* } { "-mfloat-abi=soft" } {""} } */
/* { dg-require-effective-target arm_v8_1m_mve_ok } */
/* { dg-add-options arm_v8_1m_mve } */
/* { dg-additional-options "-O2" } */

#include "arm_mve.h"

uint32_t
foo (uint32x4_t a)
{
  return vgetq_lane_u32 (a, 0);
}

/* { dg-final { scan-assembler "vmov.32"  }  } */

uint32_t
foo1 (uint32x4_t a)
{
  return vgetq_lane (a, 0);
}

/* { dg-final { scan-assembler "vmov.32"  }  } */
