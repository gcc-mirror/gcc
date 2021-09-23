/* { dg-skip-if "Incompatible float ABI" { *-*-* } { "-mfloat-abi=soft" } {""} } */
/* { dg-require-effective-target arm_v8_1m_mve_ok } */
/* { dg-add-options arm_v8_1m_mve } */
/* { dg-additional-options "-O2" } */

#include "arm_mve.h"

uint16_t
foo (uint16x8_t a)
{
  return vgetq_lane_u16 (a, 1);
}

/* { dg-final { scan-assembler "vmov.u16"  }  } */

uint16_t
foo1 (uint16x8_t a)
{
  return vgetq_lane (a, 1);
}

/* { dg-final { scan-assembler "vmov.u16"  }  } */
