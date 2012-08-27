/* { dg-do compile } */
/* This test requires widening_mul */
/* { dg-options "-mdspr2 -mgp32 -fexpensive-optimizations" } */
/* { dg-skip-if "code quality test" { *-*-* } { "-O0" } { "" } } */
/* { dg-final { scan-assembler-times "\tmadd\t\\\$ac" 3 } } */

NOMIPS16 long long
f1 (int x, int y, long long z)
{
  return (long long) x * y + z;
}

NOMIPS16 long long
f2 (int x, int y, long long z)
{
  return z + (long long) y * x;
}

NOMIPS16 long long
f3 (int x, int y, long long z)
{
  long long t = (long long) x * y;
  int temp = 5;
  if (temp == 5)
    z += t;
  return z;
}
