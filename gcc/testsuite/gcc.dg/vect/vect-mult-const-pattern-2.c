/* { dg-require-effective-target vect_int } */

#include <stdarg.h>
#include "tree-vect.h"

#define N 256

__attribute__ ((noinline)) void
foo (long long *arr)
{
  for (int i = 0; i < N; i++)
    arr[i] *= -19594LL;
}

int
main (void)
{
  check_vect ();
  long long data[N];
  int i;

  for (i = 0; i < N; i++)
    {
      data[i] = i;
      __asm__ volatile ("");
    }

  foo (data);
#pragma GCC novector
  for (i = 0; i < N; i++)
    {
      if (data[i] / -19594LL != i)
      __builtin_abort ();
      __asm__ volatile ("");
    }

  return 0;
}

/* { dg-final { scan-tree-dump {vect_recog_mult_pattern: detected:[^\n]* \* -19594} "vect" { target aarch64*-*-* xfail aarch64_sve } } } */
/* { dg-final { scan-tree-dump-times "vectorized 1 loops" 1 "vect"  { target aarch64*-*-* } } } */
