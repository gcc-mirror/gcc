/* { dg-do compile } */
/* { dg-options "-march=rv64gcv -mabi=lp64d -O3 -ftree-vectorize -mrvv-max-lmul=dynamic -fdump-tree-vect-details" } */

void
foo (int *__restrict a, int *__restrict b, int *__restrict c, int *__restrict d,
     int *__restrict e, int *__restrict f, int n)
{
  for (int i = 0; i < n; i++)
    a[i] = b[i] != f[i] ? c[i] * d[i] : e[i];
}

/* { dg-final { scan-assembler {e32,m4} } } */
/* { dg-final { scan-assembler-not {jr} } } */
/* { dg-final { scan-assembler-times {ret} 1 } } */
/* { dg-final { scan-tree-dump-times "Preferring smaller LMUL loop because it has unexpected spills" 1 "vect" } } */
/* { dg-final { scan-tree-dump-not "Maximum lmul = 16" "vect" } } */
