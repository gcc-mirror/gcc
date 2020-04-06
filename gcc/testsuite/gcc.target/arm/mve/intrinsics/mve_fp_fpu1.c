/* { dg-require-effective-target arm_v8_1m_mve_fp_ok } */
/* { dg-skip-if "Incompatible float ABI" { *-*-* } { "-mfloat-abi=soft" } {""} } */
/* { dg-additional-options "-march=armv8.1-m.main+mve.fp -mfloat-abi=hard -mthumb -mfpu=auto --save-temps" } */

#include "arm_mve.h"

int8x16_t
foo1 (int8x16_t value)
{
  int8x16_t b = value;
  return b;
}

/* { dg-final { scan-assembler "\.fpu fpv5-sp-d16" }  } */
