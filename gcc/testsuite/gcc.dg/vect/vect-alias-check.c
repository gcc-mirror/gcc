/* { dg-require-effective-target vect_int } */
/* { dg-do compile } */
/* { dg-options "-O2 -ftree-vectorize --param=vect-max-version-for-alias-checks=2 -fdump-tree-vect-details" } */

/* A test case showing three potential alias checks between
   a[i] and b[i], b[i+7], b[i+14]. With alias checks merging
   enabled, those tree checks can be merged into one, and the
   loop will be vectorized with vect-max-version-for-alias-checks=2.  */

void foo (int *a, int *b)
{
  int i;
  for (i = 0; i < 1000; ++i)
    a[i] = b[i] + b[i+7] + b[i+14];
}

/* { dg-final { scan-tree-dump-times "vectorized 1 loops" 1 "vect" } } */
/* { dg-final { cleanup-tree-dump "vect" } } */
