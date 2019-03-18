/* { dg-options "-O2 -ftree-vectorize -fdump-tree-vect-details" } */

void
f (unsigned int *restrict x, unsigned int *restrict y,
   unsigned char *restrict z, unsigned int n)
{
  for (unsigned int i = 0; i < n % 4; ++i)
    x[i] = x[i] + y[i] + z[i];
}

/* { dg-final { scan-tree-dump "not vectorized: estimated iteration count too small" vect } } */
/* { dg-final { scan-tree-dump "vectorized 0 loops" vect } } */
