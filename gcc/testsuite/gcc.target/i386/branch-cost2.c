/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-gimple -mbranch-cost=2" } */

extern int doo (void);

int
foo (int a, int b)
{
  if (a && b)
   return doo ();
  return 0;
}

/* { dg-final { scan-tree-dump-times "if " 1 "gimple" } } */
/* { dg-final { scan-tree-dump-times " & " 1 "gimple" } } */
