/* { dg-do compile } */
/* { dg-options "-march=rv64gcv -mabi=lp64d -O3 -ftree-vectorize -mrvv-max-lmul=dynamic -fdump-tree-vect-details" } */

void
f (int *restrict a, int *restrict b, int *restrict c, int *restrict d,
   int *restrict x, int n)
{
  for (int i = 0; i < n; i++)
    {
      int tmp = b[i] >> x[i];
      int tmp2 = tmp * b[i];
      c[i] = tmp2 * b[i];
      d[i] = tmp * tmp2 * b[i] >> x[i];
    }
}

void
f2 (int *restrict a, int *restrict b, int *restrict c, int *restrict d,
    int *restrict x, int n)
{
  for (int i = 0; i < n; i++)
    {
      int tmp = b[i] << x[i];
      int tmp2 = tmp * b[i];
      c[i] = tmp2 * b[i];
      d[i] = tmp * tmp2 * b[i] >> x[i];
    }
}

/* { dg-final { scan-assembler-times {e32,m4} 2 } } */
/* { dg-final { scan-assembler-not {csrr} } } */
/* { dg-final { scan-assembler-not {jr} } } */
/* { dg-final { scan-assembler-not {e32,m8} } } */
/* { dg-final { scan-assembler-not {e32,m2} } } */
/* { dg-final { scan-assembler-not {e32,m1} } } */
/* { dg-final { scan-assembler-times {ret} 2 } } */
/* { dg-final { scan-tree-dump-times "Preferring smaller LMUL loop because it has unexpected spills" 2 "vect" } } */
/* { dg-final { scan-tree-dump-times "Maximum lmul = 8" 2 "vect" } } */
/* { dg-final { scan-tree-dump-times "Maximum lmul = 4" 2 "vect" } } */
/* { dg-final { scan-tree-dump-times "Maximum lmul = 2" 2 "vect" } } */
