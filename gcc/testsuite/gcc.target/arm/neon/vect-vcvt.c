/* { dg-do compile } */
/* { dg-require-effective-target arm_neon_ok } */
/* { dg-options "-O2 -ftree-vectorize -fdump-tree-vect-details -mvectorize-with-neon-double" } */
/* { dg-add-options arm_neon } */

#define N 32

int ib[N] = {0,3,6,9,12,15,18,21,24,27,30,33,36,39,42,45,0,3,6,9,12,15,18,21,24,27,30,33,36,39,42,45};
float fa[N];
int ia[N];

int convert()
{
  int i;

  /* int -> float */
  for (i = 0; i < N; i++)
    fa[i] = (float) ib[i];

  /* float -> int */
  for (i = 0; i < N; i++)
    ia[i] = (int) fa[i];

  return 0;
}

/* { dg-final { scan-tree-dump-times "vectorized 2 loops" 1 "vect" } } */
/* { dg-final { cleanup-tree-dump "vect" } } */
