/* { dg-do compile } */
/* { dg-additional-options "--param vect-epilogues-nomask=0" } */

void foo (int * __restrict sums, int *a, int *b, int n)
{
  for (int i = 0; i < n; ++i)
    {
      sums[0] = sums[0] + a[2*i];
      sums[1] = sums[1] + a[2*i+1];
      sums[2] = sums[2] + b[2*i];
      sums[3] = sums[3] + b[2*i+1];
    }
}

/* { dg-final { scan-tree-dump-times "SLP discovery of size 2 reduction group" 2 "vect" } } */
