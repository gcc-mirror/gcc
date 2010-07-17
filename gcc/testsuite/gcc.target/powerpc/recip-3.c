/* { dg-do compile { target { { powerpc*-*-* } && { ! powerpc*-apple-darwin* } } } } */
/* { dg-options "-O2 -mrecip -ffast-math -mcpu=power7" } */
/* { dg-final { scan-assembler-times "xsrsqrtedp" 1 } } */
/* { dg-final { scan-assembler-times "xsmsub.dp" 1 } } */
/* { dg-final { scan-assembler-times "xsmuldp" 4 } } */
/* { dg-final { scan-assembler-times "xsnmsub.dp" 2 } } */
/* { dg-final { scan-assembler-times "frsqrtes" 1 } } */
/* { dg-final { scan-assembler-times "fmsubs" 1 } } */
/* { dg-final { scan-assembler-times "fmuls" 4 } } */
/* { dg-final { scan-assembler-times "fnmsubs" 2 } } */

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
