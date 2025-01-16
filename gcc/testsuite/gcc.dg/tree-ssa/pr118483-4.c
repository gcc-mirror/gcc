/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized" } */
/* PR tree-optimization/118483 */
/* { dg-final { scan-tree-dump "return 0;" "optimized" } } */

/* This should optimize down to just `return 0;` */
/* as `a == 0` and `a != 0` are opposites. */
int f(int a)
{
  return (a == 0) == (a != 0);
}
