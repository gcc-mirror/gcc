/* { dg-do compile { target { { powerpc*-*-* } && { ! powerpc*-apple-darwin* } } } } */
/* { dg-require-effective-target powerpc_fprs } */
/* { dg-skip-if "do not override -mcpu" { powerpc*-*-* } { "-mcpu=*" } { "-mcpu=power7" } } */
/* { dg-options "-O2 -mrecip -ffast-math -mcpu=power7" } */
/* { dg-final { scan-assembler-times "xsrsqrtedp\|frsqrte\ " 1 } } */
/* { dg-final { scan-assembler-times "xsmuldp\|fmul\ " 2 } } */
/* { dg-final { scan-assembler-times "xsnmsub.dp\|fnmsub\ " 2 } } */
/* { dg-final { scan-assembler-times "xsmadd.dp\|fmadd\ " 3 } } */
/* { dg-final { scan-assembler-times "xsrsqrtesp\|frsqrtes" 1 } } */
/* { dg-final { scan-assembler-times "xsmulsp\|fmuls" 2 } } */
/* { dg-final { scan-assembler-times "xsmadd.sp\|fmadds" 1 } } */
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
