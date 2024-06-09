/* { dg-add-options vect_early_break } */
/* Disabling epilogues until we find a better way to deal with scans.  */
/* { dg-do compile } */
/* { dg-additional-options "--param vect-epilogues-nomask=0" } */
/* { dg-require-effective-target vect_long } */
/* { dg-require-effective-target vect_shift } */
/* { dg-additional-options "-fno-tree-scev-cprop" } */

/* Statement used outside the loop.
   NOTE: SCEV disabled to ensure the live operation is not removed before
   vectorization.  */
__attribute__ ((noinline)) int
liveloop (int start, int n, int *x, int *y)
{
  int i = start;
  int j;
  int ret;

  for (j = 0; j < n; ++j)
    {
      i += 1;
      x[j] = i;
      ret = y[j];
    }
  return ret;
}

/* { dg-final { scan-tree-dump-times "vectorized 1 loops" 1 "vect" } } */
/* { dg-final { scan-tree-dump-times "vec_stmt_relevant_p: stmt live but not relevant" 1 "vect" } } */
