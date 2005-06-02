/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-vrp-details" } */

foo (int i, int *p)
{
  int j;

  if (i > 10)
    {
      if (p)
	{
	  j = *p;
	  /* This should be folded to if (1) because of the parent 'if
	     (p)'.  But the dereference of p above does not need an
	     assertion.  */
	  if (p)
	    return j + 1;
	}
    }
  else
    {
      j = *p - 3;
      /* This should be folded to if (0), because p has just been
	 dereferenced.  But we were not inserting enough ASSERT_EXPRs
	 to figure it out.  */
      if (!p)
	return j - 4;
    }

  return i;
}

/* { dg-final { scan-tree-dump-times "Folding predicate p_.*to 1" 1 "vrp" } } */
/* { dg-final { scan-tree-dump-times "Folding predicate p_.*to 0" 1 "vrp" } } */
/* { dg-final { scan-tree-dump-times "PREDICATE: p_\[0-9\] ne_expr 0B" 2 "vrp" } } */
/* { dg-final { cleanup-tree-dump "vrp" } } */
