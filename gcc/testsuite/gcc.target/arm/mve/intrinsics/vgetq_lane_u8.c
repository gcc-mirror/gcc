/* { dg-skip-if "Incompatible float ABI" { *-*-* } { "-mfloat-abi=soft" } {""} } */
/* { dg-require-effective-target arm_v8_1m_mve_ok } */
/* { dg-add-options arm_v8_1m_mve } */
/* { dg-additional-options "-O2" } */

#include "arm_mve.h"

uint8_t
foo (uint8x16_t a)
{
  return vgetq_lane_u8 (a, 0);
}

/* { dg-final { scan-assembler "vmov.u8"  }  } */

uint8_t
foo1 (uint8x16_t a)
{
  return vgetq_lane (a, 0);
}

/* { dg-final { scan-assembler "vmov.u8"  }  } */
