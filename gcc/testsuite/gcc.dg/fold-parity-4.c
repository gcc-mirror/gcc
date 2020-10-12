/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized" } */

int foo(unsigned int x, unsigned int y)
{
  return __builtin_parity(x) ^ __builtin_parity(y);
}

int fool(unsigned long x, unsigned long y)
{
  return __builtin_parityl(x) ^ __builtin_parityl(y);
}

int fooll(unsigned long long x, unsigned long long y)
{
  return __builtin_parityll(x) ^ __builtin_parityll(y);
}

/* { dg-final { scan-tree-dump-times "__builtin_parity" 3 "optimized" } } */

