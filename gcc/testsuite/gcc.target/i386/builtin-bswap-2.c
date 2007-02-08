/* { dg-do compile } */
/* { dg-options "-O2 -march=nocona" } */
/* { dg-final { scan-assembler-not "bswap\[ \t\]" } } */

int foo(int x)
{
  int t = __builtin_bswap32 (x);
  return __builtin_bswap32 (t);
}

