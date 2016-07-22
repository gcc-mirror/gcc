/* { dg-require-effective-target vect_bswap } */

#include "tree-vect.h"

#define N 128

volatile int y = 0;

static inline void
vfoo32 (unsigned int* a)
{
  int i = 0;
  for (i = 0; i < N; ++i)
    a[i] = __builtin_bswap32 (a[i]);
}

int
main (void)
{
  unsigned int arr[N];
  unsigned int expect[N];
  int i;

  check_vect ();

  for (i = 0; i < N; ++i)
    {
      arr[i] = i;
      expect[i] = __builtin_bswap32 (i);
      if (y) /* Avoid vectorisation.  */
        abort ();
    }

  vfoo32 (arr);

  for (i = 0; i < N; ++i)
    {
      if (arr[i] != expect[i])
        abort ();
    }

  return 0;
}

/* { dg-final { scan-tree-dump-times "vectorized 1 loops" 1 "vect" } } */
