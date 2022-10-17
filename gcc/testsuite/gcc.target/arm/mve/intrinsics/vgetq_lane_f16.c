/* { dg-skip-if "Incompatible float ABI" { *-*-* } { "-mfloat-abi=soft" } {""} } */
/* { dg-require-effective-target arm_v8_1m_mve_fp_ok } */
/* { dg-add-options arm_v8_1m_mve_fp } */
/* { dg-additional-options "-O2" } */

#include "arm_mve.h"

float16_t
foo (float16x8_t a)
{
  return vgetq_lane_f16 (a, 1);
}

/* { dg-final { scan-assembler "vmov.u16"  }  } */

float16_t
foo1 (float16x8_t a)
{
  return vgetq_lane (a, 1);
}

/* { dg-final { scan-assembler "vmov.u16"  }  } */
