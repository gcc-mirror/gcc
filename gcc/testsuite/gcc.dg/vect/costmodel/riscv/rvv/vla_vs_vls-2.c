/* { dg-do compile } */
/* { dg-options "-march=rv64gcv -mabi=lp64d -O3 -mrvv-max-lmul=m2" } */

void
foo (int *__restrict a, int *__restrict b, int *__restrict c)
{
  for (int i = 0; i < 16; i++)
    a[i] = b[i] + c[i];
}

/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*8,\s*e32,\s*m2,\s*t[au],\s*m[au]} 1 } } */
/* { dg-final { scan-assembler-times {vsetivli} 1 } } */
/* { dg-final { scan-assembler-not {vsetvli} } } */
