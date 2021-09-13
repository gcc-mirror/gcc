/* { dg-do compile } */
/* { dg-options "-O2 -fdisable-tree-evrp -fdump-tree-vrp1 -fdisable-tree-ethread -fdisable-tree-thread1" } */

int baz (void);

int
foo (int i, int j, int a)
{
  if (i >= 10)
    if (i <= 30)
      if (i == j)
	{
	  a--;

	  /* This should fold to 'if (0)'.  */
	  if (i < 0)
	    i = baz ();

	  /* This should fold to 'if (1)'.  */
	  if (j > 0)
	    a--;

	  /* This should fold to 'if (0)'.  */
	  if (i != j)
	    return 0;
	}

  return i + a + j;
}

/* { dg-final { scan-tree-dump-times "Folding predicate \[i|j\]_\[0-9\]+.*0 to 0" 1 "vrp1" } } */
/* { dg-final { scan-tree-dump-times "Folding predicate \[i|j\]_\[0-9\]+.*0 to 1" 1 "vrp1" } } */
/* { dg-final { scan-tree-dump-times "Folding predicate i_\[0-9]+.*j_\[0-9\]+.* to 0" 1 "vrp1" } } */
