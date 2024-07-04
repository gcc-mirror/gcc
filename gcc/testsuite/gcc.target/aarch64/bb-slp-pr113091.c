/* { dg-do compile } */
/* { dg-additional-options "-O3 -fdump-tree-slp-details -ftree-slp-vectorize" } */

int test(unsigned array[8]);

int foo(char *a, char *b)
{
  unsigned array[8];

  array[0] = (a[0] - b[0]);
  array[1] = (a[1] - b[1]);
  array[2] = (a[2] - b[2]);
  array[3] = (a[3] - b[3]);
  array[4] = (a[4] - b[4]);
  array[5] = (a[5] - b[5]);
  array[6] = (a[6] - b[6]);
  array[7] = (a[7] - b[7]);

  return test(array);
}

/* { dg-final { scan-tree-dump-times "Basic block will be vectorized using SLP" 1 "slp2" } } */
