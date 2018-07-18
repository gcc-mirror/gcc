/* PR77283 */
/* { dg-do compile } */
/* { dg-options "-O3 -fdump-tree-split-paths-details" } */

void
foo (double *x, double *a, double *b, long n, double limit)
{
  long i;
  for (i=0; i < n; i++)
    if (a[i] < limit)
      x[i] = b[i];
}

/* { dg-final { scan-tree-dump-times "Duplicating join block" 0 "split-paths" } } */
