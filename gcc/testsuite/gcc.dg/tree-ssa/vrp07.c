/* { dg-do compile } */
/* { dg-options "-O2 -fno-tree-fre -fdisable-tree-evrp -fdump-tree-vrp1-details -fdelete-null-pointer-checks -fno-thread-jumps" } */

int
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
/* Target disabling -fdelete-null-pointer-checks should not fold checks */
/* { dg-final { scan-tree-dump-times "Folding predicate p_.*to 1" 1 "vrp1" } } */
/* { dg-final { scan-tree-dump-times "Folding predicate p_.*to 0" 1 "vrp1" { target { ! keeps_null_pointer_checks } } } } */
/* { dg-final { scan-tree-dump-times "Folding predicate p_.*to 0" 0 "vrp1" { target {   keeps_null_pointer_checks } } } } */
