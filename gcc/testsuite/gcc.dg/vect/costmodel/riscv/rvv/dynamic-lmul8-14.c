/* { dg-do compile } */
/* { dg-options "-march=rv64gcv -mabi=lp64d -O3 -ftree-vectorize -mrvv-max-lmul=dynamic -fdump-tree-vect-details" } */

void
f (int *restrict a, int *restrict b, int *restrict c, int *restrict d, int x,
   int n)
{
  for (int i = 0; i < n; i++)
    {
      int tmp = b[i] >> x;
      int tmp2 = tmp * b[i];
      c[i] = tmp2 * b[i];
      d[i] = tmp * tmp2 * b[i] >> x;
    }
}

void
f2 (int *restrict a, int *restrict b, int *restrict c, int *restrict d, int x,
    int n)
{
  for (int i = 0; i < n; i++)
    {
      int tmp = b[i] << x;
      int tmp2 = tmp * b[i];
      c[i] = tmp2 * b[i];
      d[i] = tmp * tmp2 * b[i] >> x;
    }
}

void
f3 (int *restrict a, int *restrict b, int *restrict c, int *restrict d, int n)
{
  for (int i = 0; i < n; i++)
    {
      int tmp = b[i] >> 17;
      int tmp2 = tmp * b[i];
      c[i] = tmp2 * b[i];
      d[i] = tmp * tmp2 * b[i] >> 17;
    }
}

void
f4 (int *restrict a, int *restrict b, int *restrict c, int *restrict d, int n)
{
  for (int i = 0; i < n; i++)
    {
      int tmp = b[i] << 17;
      int tmp2 = tmp * b[i];
      c[i] = tmp2 * b[i];
      d[i] = tmp * tmp2 * b[i] >> 17;
    }
}

/* { dg-final { scan-assembler-times {e32,m8} 4 } } */
/* { dg-final { scan-assembler-not {csrr} } } */
/* { dg-final { scan-assembler-not {jr} } } */
/* { dg-final { scan-assembler-not {e32,m4} } } */
/* { dg-final { scan-assembler-not {e32,m2} } } */
/* { dg-final { scan-assembler-not {e32,m1} } } */
/* { dg-final { scan-assembler-times {ret} 4 } } */
/* { dg-final { scan-tree-dump-not "Preferring smaller LMUL loop because it has unexpected spills" "vect" } } */
/* { dg-final { scan-tree-dump-times "Maximum lmul = 8" 4 "vect" } } */
/* { dg-final { scan-tree-dump-times "Maximum lmul = 4" 4 "vect" } } */
/* { dg-final { scan-tree-dump-times "Maximum lmul = 2" 4 "vect" } } */
