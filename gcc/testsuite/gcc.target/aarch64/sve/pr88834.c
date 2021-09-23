/* { dg-do compile } */
/* { dg-options "-O3" } */

void
f (int *restrict x, int *restrict y, int *restrict z, int n)
{
  for (int i = 0; i < n; i += 2)
    {
      x[i] = y[i] + z[i];
      x[i + 1] = y[i + 1] - z[i + 1];
    }
}

/* { dg-final { scan-assembler {\tptrue\tp[0-7]\.d, all\n} } } */
/* { dg-final { scan-assembler-times {\tld1w\tz[0-9]+.s, p[0-7]/z, \[x[0-9]+, x[0-9]+, lsl 2\]\n} 2 } } */
/* { dg-final { scan-assembler-times {\tst1w\tz[0-9]+.s, p[0-7], \[x[0-9]+, x[0-9]+, lsl 2\]\n} 1 } } */
