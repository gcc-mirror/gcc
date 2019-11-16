/* { dg-options "-O2 -ftree-vectorize" } */

void
f (unsigned int *restrict x, unsigned int *restrict y,
   unsigned char *restrict z, unsigned int n)
{
  for (unsigned int i = 0; i < n % 4; ++i)
    x[i] = x[i] + y[i] + z[i];
}

/* { dg-final { scan-assembler {\tld1b\tz[0-9]+\.s, p[0-7]/z, \[x2\]\n} } } */
