/* { dg-do compile } */
/* { dg-options "-O2" } */

int
foo (int a, int b)
{
  if (a + b)
    return 5;
  else
    return 2;
  /* { dg-final { scan-assembler "cmn\tw\[0-9\]" } } */
}

typedef long long s64;

s64
foo2 (s64 a, s64 b)
{
  if (a + b)
    return 5;
  else
    return 2;
  /* { dg-final { scan-assembler "cmn\tx\[0-9\]" } } */
}  
