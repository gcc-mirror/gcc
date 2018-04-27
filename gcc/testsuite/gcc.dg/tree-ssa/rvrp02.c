/* Copy of pr21563 adjusted for rvrp.
   Make sure VRP folds the second "if" statement.  */

/* { dg-do compile } */
/* { dg-options "-O2 -fno-tree-dominator-opts -fdisable-tree-ethread -fdisable-tree-thread1 -fdisable-tree-evrp -fdump-tree-rvrp-details" } */

int
foo (int a)
{
  if (a > 1)
    if (a == 0)
      return 1;
  return 0;
}

/* { dg-final { scan-tree-dump-times "Branch rewritten" 1 "rvrp"} } */
