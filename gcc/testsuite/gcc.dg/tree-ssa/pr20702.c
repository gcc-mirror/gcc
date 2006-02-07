/* PR tree-optimization/20702
   VRP did not insert ASSERT_EXPRs into dominator dominator children
   of a basic block ending with COND_EXPR unless the children are also
   immediate successors of the basic block.  */

/* { dg-do compile } */
/* { dg-options "-O2 -fno-tree-dominator-opts -fdump-tree-vrp1-details" } */

extern void bar (int);

int
foo (int *p, int b)
{
  int a;

  if (b)
    bar (123);
  else
    bar (321);

  a = *p;
  if (p == 0)
    return 0;

  return a;
}

/* { dg-final { scan-tree-dump-times "Folding predicate" 1 "vrp1"} } */
/* { dg-final { cleanup-tree-dump "vrp1" } } */
