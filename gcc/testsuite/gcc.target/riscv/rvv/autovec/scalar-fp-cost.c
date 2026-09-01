/* { dg-do compile } */
/* { dg-options "-O3 -march=rv64gcv -mabi=lp64d -mtune=xt-c9501fdvt -fdump-tree-vect-details" } */

void
foo (float *restrict dst, const float *restrict x,
     const float *restrict y, long n)
{
  for (long i = 0; i < n; ++i)
    dst[i] = x[i] + y[i];
}

/* { dg-final { scan-tree-dump {_[0-9]+ \+ _[0-9]+ .*scalar_stmt costs 2 in body} "vect" } } */
