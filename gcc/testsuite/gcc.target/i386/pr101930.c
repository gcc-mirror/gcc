/* { dg-do compile } */
/* { dg-options "-mavx512f -O2 -mfpmath=sse -ffast-math" } */
double a;
double
__attribute__((noipa))
foo (int b)
{
  return __builtin_ldexp (a, b);
}
