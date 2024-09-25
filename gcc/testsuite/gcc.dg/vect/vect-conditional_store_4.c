/* { dg-do compile } */
/* { dg-require-effective-target vect_int } */
/* { dg-require-effective-target vect_masked_store } */

/* { dg-additional-options "-mavx2" { target avx2 } } */
/* { dg-additional-options "-march=armv9-a" { target aarch64-*-* } } */

void foo4 (signed char *restrict a, int *restrict b, int *restrict c, int *restrict d, int n, int stride)
{
  if (stride <= 1)
    return;

  for (int i = 0; i < n; i++)
    {
      int res1 = c[i];
      int res2 = d[i];
      int t = b[i+stride];
      if (a[i] > 0)
        t = res1;
      else if (a[i] < 0)
        t = res2 * 2;

      c[i] = t;
    }
}

/* { dg-final { scan-tree-dump "LOOP VECTORIZED" "vect" } } */
/* { dg-final { scan-tree-dump-times "VEC_COND_EXPR " "vect" 1 { target aarch64-*-* } } } */
