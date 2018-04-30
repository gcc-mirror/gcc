/* { dg-do compile } */
/* { dg-options "-O2 -ftree-loop-distribution -fdump-tree-ldist-details" } */

float * __restrict__ x;
float * __restrict__ y;

float foo (int n)
{
  int i;
  float tmp = 0.0;
  for (i = 0; i < n; ++i)
    {
      x[i] = 0.0;
      tmp += y[i];
    }
  return tmp;
}

/* Distributing the loop doesn't expose more parallelism.  */
/* { dg-final { scan-tree-dump-not "Loop 1 distributed: split to 2 loops" "ldist" } } */
