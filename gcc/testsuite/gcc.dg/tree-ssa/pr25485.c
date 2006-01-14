/* PR tree-optimization/25485
   VRP did not fold TRUTH_AND_EXPR.  Make sure it does now.  */

/* { dg-options "-O2 -fdump-tree-vrp" } */

int
foo (int a, int b)
{
  if (a > 50)
    return 19;
  if (a > 63 && b < 50)
    return 17;
  return 31;
}

/* { dg-final { scan-tree-dump-times "if" 1 "vrp"} } */
/* { dg-final { cleanup-tree-dump "vrp" } } */
