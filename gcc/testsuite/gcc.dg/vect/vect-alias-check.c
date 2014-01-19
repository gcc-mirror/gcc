/* { dg-do compile } */
/* { dg-require-effective-target vect_int } */
/* { dg-additional-options "--param=vect-max-version-for-alias-checks=2" } */

/* A test case showing four potential alias checks between a[i] and b[0], b[1],
   b[i+1] and b[i+2].  With alias check merging enabled, those four checks
   can be merged into two, and the loop will be vectorized with
   vect-max-version-for-alias-checks=2.  */

void foo (int *a, int *b)
{
  int i;
  for (i = 0; i < 1000; ++i)
    a[i] = b[0] + b[1] + b[i+1] + b[i+2];
}

/* { dg-final { scan-tree-dump-times "vectorized 1 loops" 1 "vect" } } */
/* { dg-final { cleanup-tree-dump "vect" } } */
