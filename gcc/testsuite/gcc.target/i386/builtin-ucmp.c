/* { dg-do compile } */
/* { dg-options "-O2 -ffast-math -mfpmath=sse -msse2" } */

double foo(double a)
{
  return __builtin_round(a);
}

/* { dg-final { scan-assembler-not "ucom" } } */
