/* { dg-do compile } */
/* { dg-options "-O2" } */
/* { dg-final { scan-assembler-not "bswapdi2" } } */

long long foo (long long x)
{
  return __builtin_bswap64 (x);
}

