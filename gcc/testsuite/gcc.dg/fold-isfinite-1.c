/* { dg-do compile } */
/* { dg-require-effective-target inf } */
/* { dg-options "-O2 -fdump-tree-optimized" } */

int foo(int x)
{
  return __builtin_finite((double)x);
}

int foof(int x)
{
  return __builtin_finitef((float)x);
}

int fool(int x)
{
  return __builtin_finitel((long double)x);
}

/* { dg-final { scan-tree-dump-times "_finite" 0 "optimized" } } */
/* { dg-final { scan-tree-dump-times " u> " 0 "optimized" } } */
