/* { dg-do compile } */
/* { dg-options "-fdump-tree-gimple" } */

double test1 (double x)
{
  return __real __builtin_cexpi (x);
}

double test2 (double x)
{
  return __imag __builtin_cexpi (x);
}

/* { dg-final { scan-tree-dump "cos" "gimple" } } */
/* { dg-final { scan-tree-dump "sin" "gimple" } } */
