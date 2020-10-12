/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-original" } */

int foo(unsigned int x)
{
  return __builtin_popcount(x) & 1;
}

int fool(unsigned long x)
{
  return __builtin_popcountl(x) & 1;
}

int fooll(unsigned long long x)
{
  return __builtin_popcountll(x) & 1;
}

/* { dg-final { scan-tree-dump-times "__builtin_popcount" 0 "original" } } */
/* { dg-final { scan-tree-dump-times "__builtin_parity" 3 "original" } } */

