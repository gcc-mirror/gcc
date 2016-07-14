/* { dg-require-effective-target vect_int } */

#include <stdarg.h>
#include "tree-vect.h"

#define N 512

/* These multiplications should be vectorizable with additions when
   no vector shift is available.  */

__attribute__ ((noinline)) void
foo (int *arr)
{
  for (int i = 0; i < N; i++)
    arr[i] *= 2;
}

__attribute__ ((noinline)) void
foo2 (int *arr)
{
  for (int i = 0; i < N; i++)
    arr[i] *= 4;
}

int
main (void)
{
  check_vect ();
  int data[N];
  int i;

  for (i = 0; i < N; i++)
    {
      data[i] = i;
      __asm__ volatile ("");
    }

  foo (data);
  for (i = 0; i < N; i++)
    {
      if (data[i] / 2 != i)
      __builtin_abort ();
      __asm__ volatile ("");
    }

  for (i = 0; i < N; i++)
    {
      data[i] = i;
      __asm__ volatile ("");
    }

  foo2 (data);
  for (i = 0; i < N; i++)
    {
      if (data[i] / 4 != i)
      __builtin_abort ();
      __asm__ volatile ("");
    }

  return 0;
}

/* { dg-final { scan-tree-dump-times "vectorized 1 loops" 2 "vect" } } */
