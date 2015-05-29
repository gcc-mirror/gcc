/* { dg-require-effective-target vect_shift } */
/* { dg-require-effective-target vect_int } */

#include "tree-vect.h"

#define N 32

unsigned short dst[N] __attribute__((aligned(N)));
unsigned short src[N] __attribute__((aligned(N)));

__attribute__ ((noinline))
void array_shift(void)
{
  int i;
  for (i = 0; i < N; i++)
    dst[i] = src[i] >> 3;
}

int main()
{
  volatile int i;
  check_vect ();

  for (i = 0; i < N; i++)
    src[i] = i << 3;

  array_shift ();

  for (i = 0; i < N; i++)
    if (dst[i] != i)
      abort ();

  return 0;
}

/* { dg-final { scan-tree-dump-times "vectorized 1 loops" 1 "vect" } } */
