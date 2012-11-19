/* { dg-do compile } */
/* { dg-options "-O2" } */

int
foo (int a, int b)
{
  return (a < b) ? 1 : 7;
  /* { dg-final { scan-assembler "csinc\tw\[0-9\].*wzr" } } */
}

typedef long long s64;

s64
foo2 (s64 a, s64 b)
{
  return (a == b) ? 7 : 1;
  /* { dg-final { scan-assembler "csinc\tx\[0-9\].*xzr" } } */
}
