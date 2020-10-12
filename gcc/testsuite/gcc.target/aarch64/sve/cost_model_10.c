/* { dg-options "-O3 -msve-vector-bits=256" } */

void
f (int *restrict x, int *restrict y, unsigned int n)
{
  for (unsigned int i = 0; i < n * 8; ++i)
    x[i] += y[i];
}

/* { dg-final { scan-assembler-not {\twhilelo\t} } } */
/* { dg-final { scan-assembler {\tptrue\tp} } } */
/* { dg-final { scan-assembler {\tcmp\tx[0-9]+, x[0-9]+\n} } } */
