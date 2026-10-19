/* { dg-do compile } */
/* { dg-options "-O2 --param tree-reassoc-width=2 -fdump-tree-reassoc2-details" } */

/* Two trapping comparisons cannot both start one sequence.  */

int
two_traps (double a, double b, double c, double d,
	   int e, int f, int g, int h)
{
  return (a < b) & (c < d) & (e < f) & (g < h);
}

/* { dg-final { scan-tree-dump-times "was chosen for reassociation" 1 "reassoc2" } } */
