/* PR tree-optimization/96232 */
/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized" } */
/* { dg-final { scan-tree-dump " \\+ -1;" "optimized" } } */
/* { dg-final { scan-tree-dump-not "~x_\[0-9]*\\\(D\\\)" "optimized" } } */

int
foo (_Bool x)
{
  return x ? 0 : -1;
}
