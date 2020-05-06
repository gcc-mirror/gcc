/* PR tree-optimization/94921 */
/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized" } */
/* { dg-final { scan-tree-dump " \[ab]_\[0-9]+\\\(D\\\) \\+ \[ab]_\[0-9]+\\\(D\\\);" "optimized" } } */
/* { dg-final { scan-tree-dump " c_\[0-9]+\\\(D\\\) - d_\[0-9]+\\\(D\\\);" "optimized" } } */
/* { dg-final { scan-tree-dump-not " ~\[abcd]\?_\[0-9]\+" "optimized" } } */

int
foo (int a, int b)
{
  return ~(~a - b);
}

int
bar (int c, int d)
{
  return ~(~c + d);
}
