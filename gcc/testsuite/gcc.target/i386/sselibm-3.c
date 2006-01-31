/* { dg-do compile } */
/* { dg-options "-O1 -msse2 -mfpmath=sse -msselibm" } */
/* { dg-require-effective-target ilp32 } */

double sin(double);
double (*mysin)(double) = sin;

double f1(double x)
{
  return sin(x);
}

double f2(double x)
{
  /* Verify we do not expand the following call to __libm_sse2_sin.  */
  return (*mysin)(x);
}

/* { dg-final { scan-assembler-times "__libm_sse2_sin" 1 } } */
