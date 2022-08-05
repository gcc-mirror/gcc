/* PR/106533 */
/* { dg-options "-O2 -fdump-tree-ldist-optimized" } */

void bar (int *a, int * __restrict b)
{
  for (int k = 0; k < 10; k++)
    {
      for (int j = 0; j < 100000; ++j)
	a[j] = b[j];
      __builtin_printf ("Foo!");
    }
}

/* The stmt with side-effects in the outer loop should not prevent
   distribution of the inner loop of the loop nest.  */
/* { dg-final { scan-tree-dump "optimized: Loop . distributed: split to 0 loops and 1 library calls" "ldist" } } */
