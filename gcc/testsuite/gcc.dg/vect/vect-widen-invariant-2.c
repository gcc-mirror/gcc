/* { dg-require-effective-target vect_int } */

#include "tree-vect.h"

#define N 64

short src[N];
int dst[N];

__attribute__ ((noipa)) void
f (int *__restrict d, const short *__restrict s, int c, int n)
{
  if (c < -100 || c > 100)
    return;
  for (const short *e = s + n; s < e; s++)
    *d++ = c * *s;
}

int
main (void)
{
  int i;

  check_vect ();

#pragma GCC novector
  for (i = 0; i < N; i++)
    src[i] = i * 1031 - 30000;

  f (dst, src, -99, N);

#pragma GCC novector
  for (i = 0; i < N; i++)
    if (dst[i] != -99 * src[i])
      abort ();
  return 0;
}

/* { dg-final { scan-tree-dump "vect_recog_widen_mult_pattern: detected" "vect" { target vect_widen_mult_hi_to_si } } } */
