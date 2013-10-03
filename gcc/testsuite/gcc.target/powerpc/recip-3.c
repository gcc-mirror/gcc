/* { dg-do compile { target { { powerpc*-*-* } && { ! powerpc*-apple-darwin* } } } } */
/* { dg-require-effective-target powerpc_fprs } */
/* { dg-options "-O2 -mrecip -ffast-math -mcpu=power7" } */
/* { dg-final { scan-assembler-times "xsrsqrtedp\|frsqrte\ " 1 } } */
/* { dg-final { scan-assembler-times "xsmsub.dp\|fmsub\ " 1 } } */
/* { dg-final { scan-assembler-times "xsmuldp\|fmul\ " 4 } } */
/* { dg-final { scan-assembler-times "xsnmsub.dp\|fnmsub\ " 2 } } */
/* { dg-final { scan-assembler-times "xsrsqrtesp\|frsqrtes" 1 } } */
/* { dg-final { scan-assembler-times "xsmsub.sp\|fmsubs" 1 } } */
/* { dg-final { scan-assembler-times "xsmulsp\|fmuls" 2 } } */
/* { dg-final { scan-assembler-times "xsnmsub.sp\|fnmsubs" 1 } } */

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
