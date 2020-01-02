/* { dg-options "-O2 -ftree-vectorize -fno-vect-cost-model -msve-vector-bits=256" } */
/* Originally from gcc.dg/vect/pr88598-4.c.  */

#define N 4

int a[N];

int __attribute__ ((noipa))
f2 (void)
{
  int b[N] = { 0, 31, 0, 31 }, res = 0;
  for (int i = 0; i < N; ++i)
    res += a[i] & b[i];
  return res;
}

/* { dg-final { scan-assembler-not {\tmov\tz[0-9]\.d, #} } } */
/* { dg-final { scan-assembler-not {\tstr\tz[0-9],} } } */
