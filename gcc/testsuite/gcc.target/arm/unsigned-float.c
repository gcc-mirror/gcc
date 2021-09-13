/* { dg-do compile } */
/* { dg-skip-if "-mpure-code supports M-profile only" { *-*-* } { "-mpure-code" } } */
/* { dg-options "-march=armv7-a+fp -mfpu=auto -O1" } */
/* Do not require arm_ok effective target to avoid skipping on arm-eabi with
   default configure options.  */
/* { dg-add-options arm_fp } */


#include <stdint.h>

double
f1 (uint16_t x)
{
  return (double)(float)x;
}

float
f2 (uint16_t x)
{
  return (float)(double)x;
}

/* { dg-final { scan-assembler-not "vcvt.(f32.f64|f64.f32)" } } */
