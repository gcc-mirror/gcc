/* { dg-do compile } */
/* { dg-options "-O -ffast-math -fdump-tree-reassoc1 -fdump-tree-optimized" } */

double foo(double a)
{
  double tmp = 5.0;
  double tmp2 = a + tmp;
  tmp2 = tmp2 - a;
  return a + tmp2 - 5.0;
}

/* { dg-final { scan-tree-dump-not "\\\+ 0.0" "reassoc1" } } */
/* { dg-final { scan-tree-dump "return a_..D.;" "optimized" } } */
/* { dg-final { cleanup-tree-dump "reassoc1" } } */
/* { dg-final { cleanup-tree-dump "optimized" } } */
