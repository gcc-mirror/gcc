/* { dg-do compile { target int128 } } */
/* { dg-options "-O2" } */

__int128 foo(long long x, unsigned long long y)
{
  return ((__int128)x<<64) | y;
}

/* { dg-final { scan-assembler-not "xorl" } } */
/* { dg-final { scan-assembler-not "orq" } } */
