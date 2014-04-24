/* { dg-require-effective-target vect_bswap } */

#include "tree-vect.h"

#define N 128

volatile int y = 0;

static inline void
vfoo16 (unsigned short int* a)
{
  int i = 0;
  for (i = 0; i < N; ++i)
    a[i] = __builtin_bswap16 (a[i]);
}

int
main (void)
{
  unsigned short arr[N];
  unsigned short expect[N];
  int i;

  for (i = 0; i < N; ++i)
    {
      arr[i] = i;
      expect[i] = __builtin_bswap16 (i);
      if (y) /* Avoid vectorisation.  */
        abort ();
    }

  vfoo16 (arr);

  for (i = 0; i < N; ++i)
    {
      if (arr[i] != expect[i])
        abort ();
    }

  return 0;
}

/* { dg-final { scan-tree-dump-times "vectorized 1 loops" 1 "vect" } } */
/* { dg-final { cleanup-tree-dump "vect" } } */
