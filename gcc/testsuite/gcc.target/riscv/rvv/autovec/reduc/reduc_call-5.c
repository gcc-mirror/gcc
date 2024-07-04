/* { dg-do compile } */
/* { dg-additional-options "-march=rv32gcv_zvfh -mabi=ilp32d -mrvv-vector-bits=scalable -fno-vect-cost-model" } */

double
foo (double *restrict r, const double *restrict a, const double *restrict b,
     const double *restrict c, double res)
{
  for (int i = 0; i < 1024; i++)
    {
      double x = __builtin_fma (a[i], b[i], c[i]);
      res += __builtin_fma (a[i], b[i], x);
    }
  return res;
}

/* { dg-final { scan-assembler-times {vfredosum\.vs\s+v[0-9]+,v[0-9]+,v[0-9]+} 1 } } */
