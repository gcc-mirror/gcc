/* PR tree-optimization/20657
   VRP did not pick up a conditional equivalence from the first "if"
   statement, which was needed to eliminate the second "if" statement.  */

/* { dg-do compile } */
/* { dg-options "-O2 -fno-tree-dominator-opts -fno-tree-fre -fdump-tree-evrp" } */

int
foo (int a)
{
  if (a == 0)
    if (a == 0)
      return 1;
  return 0;
}

/* { dg-final { scan-tree-dump-times "if" 1 "evrp"} } */
