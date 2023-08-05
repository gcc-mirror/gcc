/* { dg-additional-options "-msse4" { target sse4_runtime } } */

#include "tree-vect.h"

#define N 128

static inline void
vfoo64 (unsigned long long* a)
{
  int i = 0;
  for (i = 0; i < N; ++i)
    a[i] = __builtin_bswap64 (a[i]);
}

int
main (void)
{
  unsigned long long arr[N];
  unsigned long long expect[N];
  int i;

  check_vect ();

  for (i = 0; i < N; ++i)
    {
      arr[i] = i;
      expect[i] = __builtin_bswap64 (i);
      asm volatile ("" ::: "memory");
    }

  vfoo64 (arr);

#pragma GCC novector
  for (i = 0; i < N; ++i)
    {
      if (arr[i] != expect[i])
        abort ();
    }

  return 0;
}

/* { dg-final { scan-tree-dump-times "vectorized 1 loops" 1 "vect" { target { vect_bswap || sse4_runtime } } } } */
