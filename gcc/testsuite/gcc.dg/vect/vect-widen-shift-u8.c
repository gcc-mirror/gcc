/* { dg-require-effective-target vect_int } */
/* { dg-require-effective-target vect_shift } */

#include <stdarg.h>
#include "tree-vect.h"

#define N 64
#define C1 10
#define C2 5

__attribute__ ((noinline)) void
foo (unsigned char *src, unsigned int *dst1, unsigned int *dst2)
{
  int i;
  unsigned char b, *s = src;
  unsigned int *d1 = dst1, *d2 = dst2;

  for (i = 0; i < N; i++)
    {
      b = *s++;
      *d1 = b << C1;
      d1++;
      *d2 = b << C2;
      d2++;
    }

  s = src;
  d1 = dst1;
  d2 = dst2;
  for (i = 0; i < N; i++)
    {
      b = *s++;
      if (*d1 != b << C1 || *d2 != b << C2)
        abort ();
      d1++;
      d2++;
    }
}

int main (void)
{
  int i;
  unsigned char in[N];
  unsigned int out1[N];
  unsigned int out2[N];

  check_vect ();

  for (i = 0; i < N; i++)
    {
      in[i] = i;
      out1[i] = 255;
      out2[i] = 255;
      __asm__ volatile ("");
    }

  foo (in, out1, out2);

  return 0;
}

/* { dg-final { scan-tree-dump-times "vect_recog_widen_shift_pattern: detected" 1 "vect" { target vect_widen_shift } } } */
/* { dg-final { scan-tree-dump-times "vectorized 1 loops" 1 "vect" } } */
/* { dg-final { cleanup-tree-dump "vect" } } */

