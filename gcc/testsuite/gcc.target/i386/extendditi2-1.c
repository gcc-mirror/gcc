/* { dg-do compile { target int128 } } */
/* { dg-options "-O2" } */

__int128 foo(long long x)
{
  return (__int128)x;
}
/* { dg-final { scan-assembler "cqt?o" } } */
