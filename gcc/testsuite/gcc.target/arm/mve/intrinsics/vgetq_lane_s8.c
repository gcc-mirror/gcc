/* { dg-skip-if "Incompatible float ABI" { *-*-* } { "-mfloat-abi=soft" } {""} } */
/* { dg-require-effective-target arm_v8_1m_mve_ok } */
/* { dg-add-options arm_v8_1m_mve } */
/* { dg-additional-options "-O2" } */

#include "arm_mve.h"

int8_t
foo (int8x16_t a)
{
  return vgetq_lane_s8 (a, 0);
}

/* { dg-final { scan-assembler "vmov.s8"  }  } */

int8_t
foo1 (int8x16_t a)
{
  return vgetq_lane (a, 0);
}

/* { dg-final { scan-assembler "vmov.s8"  }  } */
