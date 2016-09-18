/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-vrp1 -fdisable-tree-ethread" } */

int
foo (int *p, int i)
{
  int x;

  if (i > 10)
    {
      if (p)
	{
	  x = *p;
	  p = 0;
	}
    }
  else
    p = 0;

  /* This should be folded to if (1), but only if we insert an
     assertion on the ELSE edge from the inner 'if (p)'.  */
  if (p == 0)
    return x + 1;

  return i;
}

/* { dg-final { scan-tree-dump-times "Folding predicate p_.*to 1" 1 "vrp1" } } */
