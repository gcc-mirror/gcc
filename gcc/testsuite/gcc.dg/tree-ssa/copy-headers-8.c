/* For targets where LOGICAL_OP_NON_SHORT_CIRCUIT evaluates to false, two
   conditional jumps are emitted instead of a combined conditional which this
   test is all about.  Thus, set it to true.  */
/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-ch2-details --param logical-op-non-short-circuit=1" } */

int is_sorted(int *a, int n, int m, int k)
{
  if (k > 0)
    for (int i = 0; i < n - 1 && m && k > i; i++)
      if (a[i] > a[i + 1])
	return 0;
  return 1;
}

/* Verify we apply loop header copying but only copy the IV tests and
   the invariant test, not the alternate exit test.  */

/* { dg-final { scan-tree-dump "is now do-while loop" "ch2" } } */
/* { dg-final { scan-tree-dump-times "Conditional combines static and invariant" 1 "ch2" } } */
/* { dg-final { scan-tree-dump-times "Will duplicate bb" 2 "ch2" } } */
