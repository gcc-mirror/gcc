/* { dg-do compile } */
/* { dg-options "-march=rv64gcv_zvl256b -mabi=lp64d -O3 -fno-cprop-registers -fno-dce -mrvv-vector-bits=scalable" } */

long
foo (long *__restrict a, long *__restrict b, long n)
{
  long i;
  for (i = 0; i < n; ++i)
    a[i] = b[i] + i * 8;
  return a[1];
}

/* { dg-final { scan-assembler-times {\tvmv1r\.v} 1 } } */
