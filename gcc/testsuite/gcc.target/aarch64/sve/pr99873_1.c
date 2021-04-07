/* { dg-options "-O3" } */

void
f (int *restrict x, int *restrict y, int *restrict z, int n)
{
  for (int i = 0; i < n; i += 3)
    {
      x[i] = y[i] + z[i];
      x[i + 1] = y[i + 1] - z[i + 1];
      x[i + 2] = y[i + 2] | z[i + 2];
    }
}

/* { dg-final { scan-assembler-times {\tld3w\t} 2 } } */
/* { dg-final { scan-assembler-times {\tst3w\t} 1 } } */
