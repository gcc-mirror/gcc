/* { dg-do compile } */
/* { dg-options "-march=rv64gcv -mabi=lp64d -O3 -ftree-vectorize -mrvv-max-lmul=dynamic -fdump-tree-vect-details" } */

/* Replaced scalar average results must not be counted as block-wide int
   live ranges.  Four independent averages expose the false spill estimate.  */
void
avg4 (unsigned char *restrict d0, const unsigned char *restrict a0,
      const unsigned char *restrict b0, unsigned char *restrict d1,
      const unsigned char *restrict a1, const unsigned char *restrict b1,
      unsigned char *restrict d2, const unsigned char *restrict a2,
      const unsigned char *restrict b2, unsigned char *restrict d3,
      const unsigned char *restrict a3, const unsigned char *restrict b3, int n)
{
  for (int i = 0; i < n; i++)
    {
      d0[i] = (a0[i] + b0[i] + 1) >> 1;
      d1[i] = (a1[i] + b1[i] + 1) >> 1;
      d2[i] = (a2[i] + b2[i] + 1) >> 1;
      d3[i] = (a3[i] + b3[i] + 1) >> 1;
    }
}

/* { dg-final { scan-assembler-times {e8,m8} 1 } } */
/* { dg-final { scan-tree-dump-not "Biggest mode = SI" "vect" } } */
/* { dg-final { scan-tree-dump-not "Preferring smaller LMUL loop because it has unexpected spills" "vect" } } */
