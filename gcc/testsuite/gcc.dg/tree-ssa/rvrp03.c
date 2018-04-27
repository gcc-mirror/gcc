/* Copied from pr25382 and adjusted for rvrp.
   PR tree-optimization/25382
   VRP used to ignore BIT_AND_EXPRs for the purpose of distilling ranges.
   Check that VRP now gets ranges from BIT_AND_EXPRs.  */

/* { dg-do compile } */
/* { dg-options "-O2 -fno-tree-ccp -fdisable-tree-evrp -fdump-tree-rvrp" } */

int
foo (int a)
{
  int b = a & 0xff;
  if (b > 300)
    return 2;
  else
    return 1;
}

/* { dg-final { scan-tree-dump-times "Branch rewritten" 1 "rvrp" } } */
