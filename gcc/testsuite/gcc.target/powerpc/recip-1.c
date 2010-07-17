/* { dg-do compile { target { { powerpc*-*-* } && { ! powerpc*-apple-darwin* } } } } */
/* { dg-options "-O2 -mrecip -ffast-math -mcpu=power6" } */
/* { dg-final { scan-assembler-times "frsqrte" 2 } } */
/* { dg-final { scan-assembler-times "fmsub" 2 } } */
/* { dg-final { scan-assembler-times "fmul" 8 } } */
/* { dg-final { scan-assembler-times "fnmsub" 4 } } */

double
rsqrt_d (double a)
{
  return 1.0 / __builtin_sqrt (a);
}

float
rsqrt_f (float a)
{
  return 1.0f / __builtin_sqrtf (a);
}
