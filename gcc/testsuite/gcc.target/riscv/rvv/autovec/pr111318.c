/* { dg-do compile } */
/* { dg-options "-march=rv64gcv -mabi=lp64d -O3 -fno-vect-cost-model" } */

void
foo (int *__restrict a, int *__restrict b, int *__restrict c, int n)
{
  for (int i = 0; i < n; i += 1)
    c[i] = a[i] + b[i];
}

/* { dg-final { scan-assembler-times {vsetvli} 1 } } */
/* { dg-final { scan-assembler-not {vsetivli} } } */
/* { dg-final { scan-assembler-times {vsetvli\s*[a-x0-9]+,\s*[a-x0-9]+} 1 } } */
/* { dg-final { scan-assembler-not {vsetvli\s*[a-x0-9]+,\s*zero} } } */
/* { dg-final { scan-assembler-not {vsetvli\s*zero} } } */
/* { dg-final { scan-assembler-not {vsetivli\s*zero} } } */
