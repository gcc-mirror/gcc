/* PR tree-optimization/22117
   VRP used think that &p[q] is nonzero even though p and q are both
   known to be zero after entering the first two "if" statements.  */

/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-vrp1" } */

void
foo (int *p, int q)
{
  if (p == 0)
    {
      if (q == 0)
	{
	  int *r = &p[q];
	  if (r != 0)
	    link_error ();
	}
    }
}

/* { dg-final { scan-tree-dump-times "Folding predicate r_.* != 0B to 0" 1 "vrp1" } } */
/* { dg-final { cleanup-tree-dump "vrp1" } } */
