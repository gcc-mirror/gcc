/* { dg-do compile } */
/* { dg-options "-std=c99 -O3" } */

void
f (int *__restrict a,
   int *__restrict b,
   int *__restrict c,
   int *__restrict d,
   int *__restrict e,
   int *__restrict f,
   int *__restrict g,
   int *__restrict h,
   int count)
{
  for (int i = 0; i < count; ++i)
    a[i] = b[i] + c[i] + d[i] + e[i] + f[i] + g[i] + h[i];
}

/* { dg-final { scan-assembler-times {\tld1w\tz[0-9]+.s, p[0-7]+/z, \[x[0-9]+, x[0-9]+, lsl 2\]\n} 7 } } */
/* { dg-final { scan-assembler-times {\tst1w\tz[0-9]+.s, p[0-7]+, \[x[0-9]+, x[0-9]+, lsl 2\]\n} 1 } } */
