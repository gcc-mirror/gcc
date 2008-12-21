/* { dg-do compile } */
/* { dg-options "-O2 -mdspr2 -mgp32" } */
/* { dg-final { scan-assembler-times "\tmsub\t\\\$ac" 2 } } */

NOMIPS16 long long
f1 (int x, int y, long long z)
{
  return z - (long long) y * x;
}

NOMIPS16 long long
f2 (int x, int y, long long z)
{
  long long t = (long long) x * y;
  int temp = 5;
  if (temp == 5)
    z -= t;
  return z;
}
