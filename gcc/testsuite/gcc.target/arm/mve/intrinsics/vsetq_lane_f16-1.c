/* { dg-skip-if "Incompatible float ABI" { *-*-* } { "-mfloat-abi=soft" } {""} } */
/* { dg-require-effective-target arm_v8_1m_mve_fp_ok } */
/* { dg-add-options arm_v8_1m_mve_fp } */
/* { dg-additional-options "-O2" } */

#include "arm_mve.h"
float16x8_t
foo (float16_t a, float16x8_t b)
{
  return vsetq_lane (23.26, b, 0);
}

/* { dg-final { scan-assembler-not "__ARM_undef" } } */
