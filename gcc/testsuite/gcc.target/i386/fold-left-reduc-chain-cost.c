/* { dg-do compile } */
/* { dg-options "-O2 -march=x86-64-v3 -fdump-tree-vect-details" } */

/* The two dependent updates form a fold-left reduction chain.  */

float
foo (float *__restrict__ a, int n)
{
  float sum = 0;
  for (int i = 0; i != n; i++)
    {
      sum += a[2 * i];
      sum += a[2 * i + 1];
    }
  return sum;
}

/* { dg-final { scan-tree-dump "Starting SLP discovery of reduction chain" "vect" } } */
/* { dg-final { scan-tree-dump "in-order FP reduction lanes" "vect" } } */
