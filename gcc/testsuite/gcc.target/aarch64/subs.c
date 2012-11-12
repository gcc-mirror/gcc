/* { dg-do compile } */
/* { dg-options "-O2" } */

int z;
int
foo (int x, int y)
{
  int l = x - y;
  if (l == 0)
    return 5;

  /* { dg-final { scan-assembler "subs\tw\[0-9\]" } } */
  z = l ;
  return 25;
}

typedef long long s64;

s64 zz;
s64
foo2 (s64 x, s64 y)
{
  s64 l = x - y;
  if (l < 0)
    return 5;

  /* { dg-final { scan-assembler "subs\tx\[0-9\]" } } */
  zz = l ;
  return 25;
}
