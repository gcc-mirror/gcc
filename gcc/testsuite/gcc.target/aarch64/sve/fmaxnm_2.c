/* { dg-options "-O2 -ftree-vectorize -fno-vect-cost-model" } */

float
f1 (float x, float *ptr)
{
  for (int i = 0; i < 128; ++i)
    x = __builtin_fmaxf (x, ptr[i]);
  return x;
}

double
f2 (double x, double *ptr)
{
  for (int i = 0; i < 128; ++i)
    x = __builtin_fmax (x, ptr[i]);
  return x;
}

/* { dg-final { scan-assembler {\twhilelo\t(p[0-7])\.s,.*\tfmaxnm\tz[0-9]+\.s, \1/m, z[0-9]+\.s, z[0-9]+\.s\n} } } */
/* { dg-final { scan-assembler-times {\tfmaxnmv\ts[0-9]+, p[0-7], z[0-9]+\.s\n} 1 } } */
/* { dg-final { scan-assembler {\twhilelo\t(p[0-7])\.d,.*\tfmaxnm\tz[0-9]+\.d, \1/m, z[0-9]+\.d, z[0-9]+\.d\n} } } */
/* { dg-final { scan-assembler-times {\tfmaxnmv\td[0-9]+, p[0-7], z[0-9]+\.d\n} 1 } } */
