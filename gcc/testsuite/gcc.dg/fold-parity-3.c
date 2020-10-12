/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized" } */

int foo(unsigned int x)
{
  return __builtin_parity(x&1);
}

int fool(unsigned long x)
{
  return __builtin_parityl(x&1);
}

int fooll(unsigned long long x)
{
  return __builtin_parityll(x&1);
}

/* { dg-final { scan-tree-dump-times "__builtin_parity" 0 "optimized" } } */

