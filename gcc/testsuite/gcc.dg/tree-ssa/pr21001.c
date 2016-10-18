/* PR tree-optimization/21001
   VRP did not insert ASSERT_EXPRs when the variable tested in a
   COND_EXPR is a single-use variable.  By propagating the definition
   of the single-use variable into the COND_EXPR, we can get useful
   range information out of the conditional.  */

/* { dg-do compile } */
/* { dg-options "-O2 -fno-tree-dominator-opts -fno-tree-fre -fdisable-tree-evrp -fdump-tree-vrp1-details" } */

int
foo (int a)
{
  int b = a != 0;
  if (b)
    if (a != 0)
      return 1;
  return 0;
}

/* { dg-final { scan-tree-dump-times "Folding predicate" 1 "vrp1"} } */
