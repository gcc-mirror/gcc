/* { dg-do compile } */
/* { dg-options "-O3 -march=rv64gcv -mabi=lp64d -fdump-tree-vect-details" } */

void __attribute__ (( target ("max-vectorization")))
foo (char *restrict a, int *restrict b, short *restrict c,
     int *restrict d, int stride)
{
  if (stride <= 1)
    return;

  for (int i = 0; i < 3; i++)
    {
      int res = c[i];
      int t = b[d[i]];
      if (a[c[i]] != 0)
        res = t * b[d[i]];
      c[i] = res;
    }
}

/* { dg-final { scan-tree-dump "vectorized 1 loops in function" "vect" } } */
