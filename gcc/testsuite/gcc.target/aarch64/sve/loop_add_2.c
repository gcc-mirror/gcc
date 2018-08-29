/* { dg-do compile } */
/* { dg-options "-std=c99 -O3" } */

void
foo (int *__restrict a, int *__restrict b)
{
  for (int i = 0; i < 512; ++i)
    a[i] += b[i];
}

/* { dg-final { scan-assembler-times {\tld1w\tz[0-9]+.s, p[0-7]+/z, \[x[0-9]+, x[0-9]+, lsl 2\]\n} 2 } } */
/* { dg-final { scan-assembler-times {\tst1w\tz[0-9]+.s, p[0-7]+, \[x[0-9]+, x[0-9]+, lsl 2\]\n} 1 } } */
