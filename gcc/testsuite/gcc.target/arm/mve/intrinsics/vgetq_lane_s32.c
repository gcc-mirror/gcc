/* { dg-skip-if "Incompatible float ABI" { *-*-* } { "-mfloat-abi=soft" } {""} } */
/* { dg-require-effective-target arm_v8_1m_mve_ok } */
/* { dg-add-options arm_v8_1m_mve } */
/* { dg-additional-options "-O2" } */

#include "arm_mve.h"

int32_t
foo (int32x4_t a)
{
  return vgetq_lane_s32 (a, 0);
}

/* { dg-final { scan-assembler "vmov.32"  }  } */

int32_t
foo1 (int32x4_t a)
{
  return vgetq_lane (a, 0);
}

/* { dg-final { scan-assembler "vmov.32"  }  } */
