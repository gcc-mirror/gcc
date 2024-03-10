/* { dg-do compile } */
/* { dg-options "-O2 -fno-tree-vrp -fdump-tree-ch2-details --param logical-op-non-short-circuit=0" } */

int is_sorted(int *a, int n, int m, int k)
{
  if (k > 0)
    for (int i = 0; k > i && m && i < n - 1 ; i++)
      if (a[i] > a[i + 1])
	return 0;
  return 1;
}

/* Verify we apply loop header copying but only copy the IV tests and
   the invariant test, not the alternate exit test.  */

/* { dg-final { scan-tree-dump "is now do-while loop" "ch2" } } */
/* { dg-final { scan-tree-dump-times "Conditional combines static and invariant" 0 "ch2" } } */
/* { dg-final { scan-tree-dump-times "Will elliminate invariant exit" 1 "ch2" } } */
/* { dg-final { scan-tree-dump-times "Will eliminate peeled conditional" 1 "ch2" } } */
/* { dg-final { scan-tree-dump-times "Will duplicate bb" 3 "ch2" } } */
