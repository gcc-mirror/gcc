/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized" } */

int foo(unsigned int x)
{
  return __builtin_parity(~x);
}

int fool(unsigned long x)
{
  return __builtin_parityl(~x);
}

int fooll(unsigned long long x)
{
  return __builtin_parityll(~x);
}

/* { dg-final { scan-tree-dump-times "~" 0 "optimized" } } */

