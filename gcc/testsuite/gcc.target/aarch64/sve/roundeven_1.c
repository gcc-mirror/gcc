/* { dg-do compile } */
/* { dg-options "-O3 -march=armv8.2-a+sve+fp16 -msve-vector-bits=scalable" } */

void
rev (double *__restrict d, double *__restrict a, int n)
{
  for (int i = 0; i < n; i++)
    d[i] = __builtin_roundeven (a[i]);
}

void
revf (float *__restrict d, float *__restrict a, int n)
{
  for (int i = 0; i < n; i++)
    d[i] = __builtin_roundevenf (a[i]);
}

void
revh (_Float16 *__restrict d, _Float16 *__restrict a, int n)
{
  for (int i = 0; i < n; i++)
    d[i] = __builtin_roundevenf16 (a[i]);
}

/* { dg-final { scan-assembler-times {\tfrintn\tz[0-9]+\.d, p[0-9]+/m, z[0-9]+\.d} 1 } } */
/* { dg-final { scan-assembler-times {\tfrintn\tz[0-9]+\.s, p[0-9]+/m, z[0-9]+\.s} 1 } } */
/* { dg-final { scan-assembler-times {\tfrintn\tz[0-9]+\.h, p[0-9]+/m, z[0-9]+\.h} 3 } } */
/* { dg-final { scan-assembler-not {\tfrintn\tv[0-9]+\.} } } */
