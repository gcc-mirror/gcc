/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-vrp" } */

foo (int k, int j)
{
  if (k >= 10)
    {
      if (j > k)
	{
	  /* We should fold this to if (1).  */
	  if (j > 0)
	    return j;
	}
    }

  return j;
}

/* { dg-final { scan-tree-dump-times "Folding predicate j_.*to 1" 1 "vrp" } } */
/* { dg-final { cleanup-tree-dump "vrp" } } */
