/* PR tree-optimization/97964 */
/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized" } */
/* { dg-final { scan-tree-dump-not "link_failure \\\(\\\);" "optimized" } } */

void link_failure (void);

void
foo (int a)
{
  long b = -2;
  int c = a > 0;
  int d = b * c;
  int e = 1 - d;
  int t = (-1 / e) == 1;
  if (t != 0)
    link_failure ();
}
