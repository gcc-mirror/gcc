/* { dg-do compile { target { powerpc*-*-* } } } */
/* { dg-skip-if "" { powerpc*-*-darwin* } } */
/* { dg-options "-O3 -ftree-vectorize -mdejagnu-cpu=power6 -ffast-math" } */
/* { dg-require-effective-target powerpc_altivec } */
/* { dg-require-effective-target powerpc_fprs } */
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
