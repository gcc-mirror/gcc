/* { dg-do run { target vect_cmdline_needed } } */
/* { dg-options "-O2 -ftree-vectorize -ftree-vectorizer-verbose=3 -fdump-tree-vect-stats" } */

#include <stdlib.h>

#define N 16

#if LONG_MAX == 2147483647
typedef short half_word;
#else
typedef int half_word;
#endif

int main ()
{
  int i;
  half_word ia[N];
  half_word ic[N] = {0,3,6,9,12,15,18,21,24,27,30,33,36,39,42,45};
  half_word ib[N] = {0,3,6,9,12,15,18,21,24,27,30,33,36,39,42,45};

  /* Not worthwhile, only 2 parts per int */
  for (i = 0; i < N; i++)
    {
      ia[i] = ib[i] + ic[i];
    }

  /* check results:  */
  for (i = 0; i < N; i++)
    {
      if (ia[i] != ib[i] + ic[i])
        abort ();
    }

  return 0;
}


/* { dg-final { scan-tree-dump-times "vectorized 0 loops" 1 "vect" } } */
/* { dg-final { cleanup-tree-dump "vect" } } */
