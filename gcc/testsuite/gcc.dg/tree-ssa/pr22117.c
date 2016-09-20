/* PR tree-optimization/22117
   VRP used think that &p[q] is nonzero even though p and q are both
   known to be zero after entering the first two "if" statements.  */

/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-vrp1" } */

void link_error (void);

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

/* { dg-final { scan-tree-dump-times "link_error" 0 "vrp1" } } */
