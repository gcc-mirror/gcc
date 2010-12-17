/* { dg-do run { target vect_cmdline_needed } } */
/* { dg-options "-O2 -ftree-vectorize -ftree-vectorizer-verbose=3 -fdump-tree-vect-stats" } */
/* { dg-options "-O2 -ftree-vectorize -ftree-vectorizer-verbose=3 -fdump-tree-vect-stats -mno-sse" { target { i?86-*-* x86_64-*-* } } } */

#include <stdlib.h>

#define N 16

#if __INT_MAX__ == 32767
typedef char half_word;
#elif __LONG_MAX__ == 2147483647
typedef short half_word;
#else
typedef int half_word;
#endif

half_word ic[N] = {0,3,6,9,12,15,18,21,24,27,30,33,36,39,42,45};
half_word ib[N] = {0,3,6,9,12,15,18,21,24,27,30,33,36,39,42,45};

int main ()
{
  int i;
  half_word ia[N];

  for (i = 0; i < N; i++)
    {
      ia[i] = ib[i] & ic[i];
    }

  /* check results:  */
  for (i = 0; i < N; i++)
    {
      if (ia[i] != (ib[i] & ic[i]))
        abort ();
    }

  return 0;
}


/* { dg-final { scan-tree-dump-times "vectorized 1 loops" 1 "vect" { target { ! avr-*-* } } } } */
/* { dg-final { cleanup-tree-dump "vect" } } */
