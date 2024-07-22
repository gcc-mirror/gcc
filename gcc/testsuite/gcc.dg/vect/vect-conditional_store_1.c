/* { dg-do compile } */
/* { dg-require-effective-target vect_int } */
/* { dg-require-effective-target vect_masked_store } */

/* { dg-additional-options "-mavx2" { target avx2 } } */
/* { dg-additional-options "-march=armv9-a" { target aarch64-*-* } } */

void foo1 (char *restrict a, int *restrict b, int *restrict c, int n, int stride)
{
  if (stride <= 1)
    return;

  for (int i = 0; i < n; i++)
    {
      int res = c[i];
      int t = b[i+stride];
      if (a[i] != 0)
        res = t;
      c[i] = res;
    }
}

/* { dg-final { scan-tree-dump "LOOP VECTORIZED" "vect" } } */
/* { dg-final { scan-tree-dump-not "VEC_COND_EXPR " "vect" { target aarch64-*-* } } } */
