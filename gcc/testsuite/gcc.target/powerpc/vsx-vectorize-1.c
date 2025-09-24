/* { dg-do compile } */
/* { dg-skip-if "" { powerpc*-*-darwin* } } */
/* { dg-options "-mdejagnu-cpu=power7 -O2 -ftree-vectorize -fno-tree-loop-distribute-patterns -fno-vect-cost-model -fdump-tree-vect-details" } */
/* { dg-require-effective-target powerpc_vsx } */

/* Taken from vect/vect-align-1.c.  */

/* Compile time known misalignment. Cannot use loop peeling to align
   the store.  */

#define N 16

struct foo {
  char x;
  int y[N];
} __attribute__((packed));

int x[N] = {0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15};

__attribute__ ((noinline)) int
main1 (struct foo * __restrict__ p)
{
  int i;

  for (i = 0; i < N; i++)
    {
      p->y[i] = x[i];
    }
  return 0;
}

/* { dg-final { scan-tree-dump-times "Vectorizing an unaligned access" 0 "vect" } } */
/* { dg-final { scan-tree-dump-times "Alignment of access forced using versioning" 1 "vect" } } */
/* { dg-final { scan-tree-dump-times "vectorized 1 loops" 1 "vect" } } */
