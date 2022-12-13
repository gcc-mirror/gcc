/* { dg-require-effective-target powerpc_p9vector_ok } */
/* { dg-require-effective-target lp64 } */
/* { dg-options "-mdejagnu-cpu=power9 -O2 -ftree-vectorize -fno-vect-cost-model -funroll-loops -fno-tree-loop-distribute-patterns --param vect-partial-vector-usage=2 -fdump-tree-optimized" } */

/* Verify there is only one IFN call LEN_LOAD and IFN_STORE separately.  */

#define N 16
int src[N];
int dest[N];

void
foo ()
{
  for (int i = 0; i < (N - 1); i++)
    dest[i] = src[i];
}

/* { dg-final { scan-tree-dump-times {\mLEN_LOAD\M} 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times {\mLEN_STORE\M} 1 "optimized" } } */
