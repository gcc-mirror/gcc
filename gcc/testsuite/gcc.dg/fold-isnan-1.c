/* { dg-do compile } */
/* { dg-require-effective-target inf } */
/* { dg-options "-O2 -fdump-tree-optimized" } */

int foo(int x)
{
  return __builtin_isnan((double)x);
}

int foof(int x)
{
  return __builtin_isnanf((float)x);
}

int fool(int x)
{
  return __builtin_isnanl((long double)x);
}

/* { dg-final { scan-tree-dump-times "_isnan" 0 "optimized" } } */
/* { dg-final { scan-tree-dump-times " unord " 0 "optimized" } } */
