/* { dg-do compile } */
/* { dg-options "-march=rv64gcv -mabi=lp64d -O3 -fno-vect-cost-model" } */

void
foo (int *__restrict a, int *__restrict b, int *__restrict c,
     int *__restrict a2, int *__restrict b2, int *__restrict c2,
     int *__restrict a3, int *__restrict b3, int *__restrict c3,
     int *__restrict a4, int *__restrict b4, int *__restrict c4,
     int *__restrict a5, int *__restrict b5, int *__restrict c5,
     int *__restrict d, int *__restrict d2, int *__restrict d3,
     int *__restrict d4, int *__restrict d5, int n, int m)
{
  for (int i = 0; i < n; i++)
    {
      a[i] = b[i] + c[i];
      a2[i] = b2[i] + c2[i];
      a3[i] = b3[i] + c3[i];
      a4[i] = b4[i] + c4[i];
      a5[i] = a[i] + a4[i];
      d[i] = a[i] - a2[i];
      d2[i] = a2[i] * a[i];
      d3[i] = a3[i] * a2[i];
      d4[i] = a2[i] * d2[i];
      d5[i] = a[i] * a2[i] * a3[i] * a4[i] * d[i];
    }
}

/* { dg-final { scan-assembler-times {vsetvli} 1 } } */
/* { dg-final { scan-assembler-not {vsetivli} } } */
/* { dg-final { scan-assembler-times {vsetvli\s*[a-x0-9]+,\s*[a-x0-9]+} 1 } } */
/* { dg-final { scan-assembler-not {vsetvli\s*[a-x0-9]+,\s*zero} } } */
/* { dg-final { scan-assembler-not {vsetvli\s*zero} } } */
/* { dg-final { scan-assembler-not {vsetivli\s*zero} } } */
