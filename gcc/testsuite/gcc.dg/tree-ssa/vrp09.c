/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-vrp1" } */

foo (int *p)
{
  int x = baz ();

  if (p == 0)
    goto L78;
  else
    {
      x = *p;
      /* This should be folded to if (1).  */
      if (p)
	x = x + 1;
L78:
      /* This should not be folded to if (1).  */
      if (p)
	{
	  x = baz (*p);
	  /* This should be folded to if (1).  */
	  if (p)
	    return x + 3;
	}

      return x - 3;
    }
}

/* { dg-final { scan-tree-dump-times "Folding predicate p_.. != 0B to 1" 2 "vrp1" } } */
/* { dg-final { cleanup-tree-dump "vrp1" } } */
