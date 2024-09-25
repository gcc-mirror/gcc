/* { dg-do compile } */
/* { dg-additional-options "-march=armv9-a -Ofast -fdump-tree-vect-details" } */

void
foo (char *restrict a, int *restrict b, int *restrict c, int n, int stride)
{
  if (stride <= 1)
    return;

  for (int i = 0; i < 9; i++)
    {
      int res = c[i];
      int t = b[i*stride];
      if (a[i] != 0)
        res = t;
      c[i] = res;
    }
}

/* { dg-final { scan-tree-dump-not "LOOP VECTORIZED" "vect" } } */
