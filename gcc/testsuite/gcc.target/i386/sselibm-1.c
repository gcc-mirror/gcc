/* { dg-do compile } */
/* { dg-options "-msse2 -mfpmath=sse" } */
/* { dg-require-effective-target ilp32 } */

double sin(double);

double foo(double x)
{
  return sin(x);
}

/* { dg-final { scan-assembler-not "__libm_sse2_sin" } } */
