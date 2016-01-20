/* { dg-do compile } */
/* { dg-require-effective-target arm_fp_ok } */
/* { dg-skip-if "need fp instructions" { *-*-* } { "-mfloat-abi=soft" } { "" } } */
/* { dg-options "-march=armv7-a -O1" } */
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
