/* { dg-do run { target vect_cmdline_needed } } */
/* { dg-options "-O2 -ftree-vectorize -ftree-vectorizer-verbose=4 -fdump-tree-vect-stats" } */
/* { dg-options "-O2 -ftree-vectorize -ftree-vectorizer-verbose=4 -fdump-tree-vect-stats -mno-sse" { target { i?86-*-* x86_64-*-* } } } */

#include <stdlib.h>

#define N 128
#define OFF 3

/* unaligned store.  */

int main_1 (int off)
{
  int i;
  char ia[N+OFF];

  for (i = 0; i < N; i++)
    {
      ia[i+off] = 5;
    }

  /* check results:  */
  for (i = 0; i < N; i++)
    {
      if (ia[i+off] != 5)
        abort ();
    }

  return 0;
}

static volatile int off = 1;

int main (void)
{
  return main_1 (off);
}


/* { dg-final { scan-tree-dump-times "vectorized 1 loops" 1 "vect"  } } */
/* { dg-final { scan-tree-dump-times "Vectorizing an unaligned access" 0 "vect" } } */
/* { dg-final { scan-tree-dump-times "Alignment of access forced using peeling" 1 "vect" } } */
/* { dg-final { cleanup-tree-dump "vect" } } */
