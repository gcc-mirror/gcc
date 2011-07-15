/* { dg-do compile } */
/* { dg-options "-O2 -march=c64x+" } */
/* { dg-final { scan-assembler-not "call" } } */

int foo (int x)
{
  return __builtin_bswap32 (x);
}

long long bar (long long x)
{
  return __builtin_bswap64 (x);
}
