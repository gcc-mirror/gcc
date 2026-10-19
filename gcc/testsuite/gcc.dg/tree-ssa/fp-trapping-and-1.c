/* { dg-do compile } */
/* { dg-options "-O2 -ftrapping-math -fdump-tree-optimized" } */

int
f (double a, double b)
{
  return (a > b) & (a < b);
}

/* { dg-final { scan-tree-dump-times " > " 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times " < " 1 "optimized" } } */
