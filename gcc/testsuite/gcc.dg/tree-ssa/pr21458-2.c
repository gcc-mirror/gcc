/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-evrp-details" } */

/* range_of_stmt will call SCEV to set global ranges for the PHI
   results, and alter the results here.  */
/* { dg-additional-options "-fdisable-tree-rvrp1 -fdisable-tree-rvrp2" } */

extern void g (void);
extern void bar (int);

int
foo (int a)
{
  int i;

  for (i = 1; i < 100; i++)
    {
      if (i)
	g ();
    }
}

/* { dg-final { scan-tree-dump-times "Predicate evaluates to: 1" 1 "evrp" } } */
