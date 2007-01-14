/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-vrp1 -fno-early-inlining" } */

inline int
bounce (int arg)
{
  return arg;
}

foo (int k, int j, int z)
{
  if (k > z)
    {
      if (j > k)
	{
	  /* We should fold this to if (1).  */
	  if (j > bounce (z))
	    return j;
	}
    }

  return j+1;
}

/* { dg-final { scan-tree-dump-times "Folding predicate.*to 1" 1 "vrp1" } } */
/* { dg-final { cleanup-tree-dump "vrp1" } } */
