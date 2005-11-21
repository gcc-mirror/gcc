/* { dg-do compile } */
/* { dg-options "-ffinite-math-only -fdump-tree-gimple" } */

double f(double x)
{
  return x / x;
}

/* Division should be turned into 1.0.  */

/* { dg-final { scan-tree-dump-not " / " "gimple" } } */
/* { dg-final { cleanup-tree-dump "gimple" } } */

