/* { dg-do compile } */
/* { dg-options "-O -fdump-tree-optimized -fdump-tree-original" } */

double foo(double x, double y)
{
  return ((x + 0.1234 * y) * (x - 0.1234 * y));
}

/* Keep positive constants during folding.  */
/* { dg-final { scan-tree-dump-times " 1.23" 2 "original" } } */
/* CSE one multiplication.  */
/* { dg-final { scan-tree-dump-times " \\\* " 2 "optimized" } } */
