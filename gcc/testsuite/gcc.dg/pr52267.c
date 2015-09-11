/* PR tree-optimization/52267 */
/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized" } */

int
foo (int a, int b)
{
  if (a > 3 || a < 0)
    return a;
  a &= 3;
  return a & 3;
}

int
bar (int a)
{
  if (a & ~3)
    return a;
  return a & 3;
}

/* { dg-final { scan-tree-dump-not "& 3" "optimized" } } */
/* { dg-final { scan-tree-dump-not "& -4" "optimized" } } */
