/* PR tree-optimization/25382
   VRP used to ignore BIT_AND_EXPRs for the purpose of distilling ranges.
   Check that VRP now gets ranges from BIT_AND_EXPRs.  */

/* { dg-do compile } */
/* { dg-options "-O2 -fno-thread-jumps -fno-tree-ccp -fdisable-tree-evrp -fdump-tree-vrp1" } */

int
foo (int a)
{
  int b = a & 0xff;
  if (b > 300)
    return 2;
  else
    return 1;
}

/* { dg-final { scan-tree-dump-times "Folding predicate b_.* > 300 to 0" 1 "vrp1" } } */
