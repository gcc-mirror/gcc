/* { dg-do compile } */
/* { dg-options "-O2 -fno-strict-aliasing -ftree-loop-distribution -fdump-tree-ldist-details" } */

struct desc {
  int i;
  void * __restrict__ data;
  int j;
} a, b;

float foo (int n)
{
  int i;
  float * __restrict__ x, * __restrict__ y, tmp = 0.0;
  x = (float * __restrict__)a.data;
  y = (float * __restrict__)b.data;
  for (i = 0; i < n; ++i)
    {
      x[i] = 0.0;
      tmp += y[i];
    }
  return tmp;
}

/* We should apply loop distribution.  */

/* { dg-final { scan-tree-dump "Loop 1 distributed: split to 2 loops" "ldist" } } */
