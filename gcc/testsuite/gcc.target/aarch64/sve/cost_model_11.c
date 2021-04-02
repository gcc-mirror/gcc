/* { dg-options "-O2 -ftree-vectorize -msve-vector-bits=128" } */

long
f (long *x, long *y, long *z, long n)
{
  long res = 0;
  for (long i = 0; i < n; ++i)
    z[i] = x[i * 4] + y[i * 4];
  return res;
}

/* { dg-final { scan-assembler-not {\tld4d\t} } } */
