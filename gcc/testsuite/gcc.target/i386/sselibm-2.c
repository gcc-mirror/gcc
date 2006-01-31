/* { dg-do compile } */
/* { dg-options "-msse2 -mfpmath=sse -msselibm" } */
/* { dg-require-effective-target ilp32 } */

double sin(double);

double foo(double x)
{
  return sin(x);
}

/* { dg-final { scan-assembler "__libm_sse2_sin" } } */
