/* { dg-do compile { target { powerpc*-*-* } } } */
/* { dg-skip-if "" { powerpc*-*-darwin* } { "*" } { "" } } */
/* { dg-require-effective-target powerpc_altivec_ok } */
/* { dg-require-effective-target powerpc_fprs } */
/* { dg-skip-if "do not override -mcpu" { powerpc*-*-* } { "-mcpu=*" } { "-mcpu=power6" } } */
/* { dg-options "-O3 -ftree-vectorize -mcpu=power6 -ffast-math" } */
/* { dg-final { scan-assembler-times "fmadd" 1 } } */
/* { dg-final { scan-assembler-times "fmsub " 1 } } */
/* { dg-final { scan-assembler-not "fmul" } } */
/* { dg-final { scan-assembler-not "fadd " } } */

/* Check whether the common FFT idiom (a*b)+c and (a*b)-c generates two fma
   instructions, instead of a multiply, add, and subtract.  */

void
fft (double *result, double a, double b, double c)
{
  result[0] = (a*b) + c;
  result[1] = (a*b) - c;
}
