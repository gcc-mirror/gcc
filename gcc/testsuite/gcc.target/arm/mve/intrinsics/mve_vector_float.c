/* { dg-require-effective-target arm_v8_1m_mve_fp_ok } */
/* { dg-add-options arm_v8_1m_mve_fp } */
/* { dg-skip-if "Incompatible float ABI" { *-*-* } { "-mfloat-abi=soft" } {""} } */

#include "arm_mve.h"

float32x4_t
foo32 (float32x4_t value)
{
  float32x4_t b = value;
  return b;
}

float16x8_t
foo16 (float16x8_t value)
{
  float16x8_t b = value;
  return b;
}
