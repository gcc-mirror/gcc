/* { dg-skip-if "Incompatible float ABI" { *-*-* } { "-mfloat-abi=soft" } {""} } */
/* { dg-require-effective-target arm_v8_1m_mve_fp_ok } */
/* { dg-add-options arm_v8_1m_mve_fp } */
/* { dg-additional-options "-O2" } */

#include "arm_mve.h"

float32_t
foo (float32x4_t a)
{
  return vgetq_lane_f32 (a, 0);
}

/* { dg-final { scan-assembler "vmov.32"  }  } */

float32_t
foo1 (float32x4_t a)
{
  return vgetq_lane (a, 0);
}

/* { dg-final { scan-assembler "vmov.32"  }  } */
