/* { dg-options "-O2 -ftree-vectorize" } */

void
f (char *restrict x, char *restrict y, char *restrict z, unsigned long n)
{
  for (unsigned long i = 0; i < n; i += 1)
    x[i] = y[i] + z[i];
}

/* { dg-final { scan-assembler-not {\tptest\t} } } */
