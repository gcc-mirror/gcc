/* { dg-options "-O2 -ftree-vectorize -fno-vect-cost-model" } */

void
f (double *restrict res, double *restrict ptr)
{
  double x0 = res[0];
  double x1 = res[1];
  for (int i = 0; i < 128; i += 2)
    {
      x0 = __builtin_fmax (x0, ptr[i + 0]);
      x1 = __builtin_fmax (x1, ptr[i + 1]);
    }
  res[0] = x0;
  res[1] = x1;
}

/* { dg-final { scan-assembler {\twhilelo\t(p[0-7])\.d,.*\tfmaxnm\tz[0-9]+\.d, \1/m, z[0-9]+\.d, z[0-9]+\.d\n} } } */
/* { dg-final { scan-assembler-times {\tfmaxnmv\td[0-9]+, p[0-7], z[0-9]+\.d\n} 2 } } */
