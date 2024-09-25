/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized" } */

int foo(int x)
{
  return __builtin_ctz (-x);
}

/* { dg-final { scan-tree-dump-not "-x_" "optimized"} } */
