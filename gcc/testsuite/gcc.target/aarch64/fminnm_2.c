/* { dg-options "-O2 -ftree-vectorize -fno-vect-cost-model" } */

#pragma GCC target "+nosve"

void
f (double *restrict res, double *restrict ptr)
{
  double x0 = res[0];
  double x1 = res[1];
  for (int i = 0; i < 128; i += 2)
    {
      x0 = __builtin_fmin (x0, ptr[i + 0]);
      x1 = __builtin_fmin (x1, ptr[i + 1]);
    }
  res[0] = x0;
  res[1] = x1;
}

/* { dg-final { scan-assembler-times {\tfminnm\tv[0-9]+\.2d, v[0-9]+\.2d, v[0-9]+\.2d\n} 1 } } */
/* { dg-final { scan-assembler {\tstr\tq[0-9]+, \[x0\]\n} } } */
