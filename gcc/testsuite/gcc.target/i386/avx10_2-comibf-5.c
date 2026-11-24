/* { dg-do compile } */
/* { dg-options "-march=x86-64-v3 -mavx10.2 -O2 -fno-trapping-math" } */
/* { dg-final { scan-assembler-not "vcomisbf16" } } */
/* { dg-final { scan-assembler-times "vucomxss\[ \\t\]+\[^{}\n\]*%xmm\[0-9\]+(?:\n|\[ \\t\]+#)" 4 } } */

int
eq (__bf16 a, __bf16 b)
{
  return a == b;
}

int
ne (__bf16 a, __bf16 b)
{
  return a != b;
}

double
sel (__bf16 a, __bf16 b, double x, double y)
{
  return a == b ? x : y;
}

void
br (__bf16 a, __bf16 b, int *p)
{
  if (a != b)
    *p = 1;
}
