/* { dg-do compile } */
/* { dg-options "-march=rv64gcv -mabi=lp64d -O3 -ftree-vectorize -mrvv-max-lmul=m1" } */

void
foo (char *__restrict a, short *__restrict b, int n)
{
  for (int i = 0; i < n; i++)
    b[i] = (short) a[i];
}

/* { dg-final { scan-assembler-times {vsetvli\s+[a-x0-9]+,\s*[a-x0-9]+,\s*e16,\s*m1,\s*t[au],\s*m[au]} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli} 1 } } */
