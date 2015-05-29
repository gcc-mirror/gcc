/* { dg-require-effective-target vect_int } */
/* { dg-require-effective-target vect_shift } */

#include <stdarg.h>
#include "tree-vect.h"

#define N 64
#define C 16

__attribute__ ((noinline)) void
foo (short *src, int *dst)
{
  int i;
  short b, b0, b1, b2, b3, *s = src;
  int *d = dst;

  for (i = 0; i < N/4; i++)
    {
      b0 = *s++;
      b1 = *s++;
      b2 = *s++;
      b3 = *s++;
      *d = b0 << C;
      d++;
      *d = b1 << C;
      d++;
      *d = b2 << C;
      d++;
      *d = b3 << C;
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

  s = src;
  d = dst;
  for (i = 0; i < N/4; i++)
    {
      b0 = *s++;
      b1 = *s++;
      b2 = *s++;
      b3 = *s++;
      *d = b0 << C;
      d++;
      *d = b1 << C;
      d++;
      *d = b2 << C;
      d++;
      *d = b3 << 6;
      d++;
    }

  s = src;
  d = dst;
  for (i = 0; i < N/4; i++)
    {
      b = *s++;
      if (*d != b << C)
        abort ();
      d++;
      b = *s++;
      if (*d != b << C)
        abort ();
      d++;
      b = *s++;
      if (*d != b << C)
        abort ();
      d++;
      b = *s++;
      if (*d != b << 6)
        abort ();
      d++;
    }
}

int main (void)
{
  int i;
  short in[N];
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

/* { dg-final { scan-tree-dump-times "vect_recog_widen_shift_pattern: detected" 8 "vect" { target vect_widen_shift } } } */
/* { dg-final { scan-tree-dump-times "vectorized 2 loops" 1 "vect" } } */

