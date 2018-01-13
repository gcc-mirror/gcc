/* { dg-do compile } */
/* { dg-options "-O2 -ftree-vectorize" } */

void __attribute__ ((noinline, noclone))
unpack_float_plus_7point9 (double *d, float *s, int size)
{
  for (int i = 0; i < size; i++)
    d[i] = s[i] + 7.9;
}

/* { dg-final { scan-assembler-times {\tuunpklo\tz[0-9]+\.d, z[0-9]+\.s\n} 1 } } */
/* { dg-final { scan-assembler-times {\tuunpkhi\tz[0-9]+\.d, z[0-9]+\.s\n} 1 } } */
/* { dg-final { scan-assembler-times {\tfcvt\tz[0-9]+\.d, p[0-7]/m, z[0-9]+\.s\n} 2 } } */
