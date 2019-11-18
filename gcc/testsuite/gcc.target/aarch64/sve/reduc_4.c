/* { dg-do compile } */
/* { dg-options "-O2 -ftree-vectorize -ffast-math --param aarch64-sve-compare-costs=0" } */

double
f (double *restrict a, double *restrict b, int *lookup)
{
  double res = 0.0;
  for (int i = 0; i < 512; ++i)
    res += a[lookup[i]] * b[i];
  return res;
}

/* { dg-final { scan-assembler-times {\tfmla\tz[0-9]+.d, p[0-7]/m, } 2 } } */
/* Check that the vector instructions are the only instructions.  */
/* { dg-final { scan-assembler-times {\tfmla\t} 2 } } */
/* { dg-final { scan-assembler-not {\tfadd\t} } } */
/* { dg-final { scan-assembler-times {\tfaddv\td0,} 1 } } */
/* { dg-final { scan-assembler-not {\tsel\t} } } */
