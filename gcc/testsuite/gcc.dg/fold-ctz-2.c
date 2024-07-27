/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized" } */

int foo(int x)
{
  return __builtin_ctz (__builtin_abs (x));
}

/* { dg-final { scan-tree-dump-not "ABS_EXPR" "optimized"} } */
