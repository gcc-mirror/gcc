/* { dg-options "-O3" } */

#pragma GCC target "+nosve"

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

/* { dg-final { scan-assembler-times {\tld3\t} 2 } } */
/* { dg-final { scan-assembler-times {\tst3\t} 1 } } */
