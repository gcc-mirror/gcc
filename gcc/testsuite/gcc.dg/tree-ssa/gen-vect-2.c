/* { dg-do run { target vect_cmdline_needed } } */
/* { dg-options "-O2 -ftree-vectorize -ftree-vectorizer-verbose=4 -fdump-tree-vect-stats" } */
/* { dg-options "-O2 -ftree-vectorize -ftree-vectorizer-verbose=4 -fdump-tree-vect-stats -mno-sse" { target { i?86-*-* x86_64-*-* } } } */

#include <stdlib.h>

#define N 16
 
#if __INT_MAX__ == 32767
typedef char half_word;
#elif __LONG_MAX__ == 2147483647
typedef short half_word;
#else
typedef int half_word;
#endif

int main ()
{
  half_word cb[N] = {0,3,6,9,12,15,18,21,24,27,30,33,36,39,42,45};
  half_word ca[N];
  int i;

  for (i = 0; i < N; i++)
    {
      ca[i] = cb[i];
    }

  /* check results:  */
  for (i = 0; i < N; i++)
    {
      if (ca[i] != cb[i])
        abort ();
    }

  return 0;
}

/* { dg-final { scan-tree-dump-times "vectorized 1 loops" 1 "vect" } } */
/* { dg-final { scan-tree-dump-times "Vectorizing an unaligned access" 0 "vect" } } */
/* { dg-final { cleanup-tree-dump "vect" } } */
