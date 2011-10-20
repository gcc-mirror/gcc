/* { dg-require-effective-target vect_int } */
/* { dg-require-effective-target vect_shift } */

#include <stdarg.h>
#include "tree-vect.h"

#define N 64
#define C 12

__attribute__ ((noinline)) void
foo (char *src, int *dst)
{
  int i;
  char b, *s = src;
  int *d = dst;

  for (i = 0; i < N; i++)
    {
      b = *s++;
      *d = b << C;
      d++;
    }

  s = src;
  d = dst;
  for (i = 0; i < N; i++)
    {
      b = *s++;
      if (*d != b << C)
        abort ();
      d++;
    }
}

int main (void)
{
  int i;
  char in[N];
  int out[N];

  check_vect ();

  for (i = 0; i < N; i++)
    {
      in[i] = i;
      out[i] = 255;
      __asm__ volatile ("");
    }

  foo (in, out);

  return 0;
}

/* { dg-final { scan-tree-dump-times "vect_recog_widen_shift_pattern: detected" 1 "vect" { target vect_widen_shift } } } */
/* { dg-final { scan-tree-dump-times "vectorized 1 loops" 1 "vect" } } */
/* { dg-final { cleanup-tree-dump "vect" } } */

