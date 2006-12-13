/* { dg-do compile } */
/* { dg-options "-fdump-tree-gimple" } */

double test (double x)
{
  double s, c;
  __builtin_sincos (x, &s, &c);
  return s + c;
}

/* { dg-final { scan-tree-dump "__builtin_cexpi" "gimple" } } */
/* { dg-final { scan-tree-dump-not "sincos" "gimple" } } */
/* { dg-final { cleanup-tree-dump "gimple" } } */
