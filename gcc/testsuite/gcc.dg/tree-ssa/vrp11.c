/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-vrp" } */

foo (int k, int j, int z)
{
  if (k > z)
    {
      if (j > k)
	{
	  /* We should fold this to if (1).  */
	  if (j > z)
	    return j;
	}
    }

  return j;
}

/* { dg-final { scan-tree-dump-times "Folding predicate.*to 1" 1 "vrp" } } */
/* { dg-final { cleanup-tree-dump "vrp" } } */
