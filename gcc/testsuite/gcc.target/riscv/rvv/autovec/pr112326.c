/* { dg-do compile } */
/* { dg-options "-march=rv64gcv -mabi=lp64d -O3 -fno-vect-cost-model -mrvv-vector-bits=zvl" } */

void
f (int *__restrict y, int *__restrict x, int *__restrict z, int n)
{
  for (int i = 0; i < n; ++i)
    x[i] = y[i] + x[i];
}

/* { dg-final { scan-assembler-times {vsetvli} 1 } } */
/* { dg-final { scan-assembler-not {vsetivli} } } */
/* { dg-final { scan-assembler-times {vsetvli\s*[a-x0-9]+,\s*[a-x0-9]+} 1 } } */
/* { dg-final { scan-assembler-not {vsetvli\s*[a-x0-9]+,\s*zero} } } */
/* { dg-final { scan-assembler-not {vsetvli\s*zero} } } */
/* { dg-final { scan-assembler-not {vsetivli\s*zero} } } */
