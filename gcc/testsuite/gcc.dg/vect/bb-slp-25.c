/* { dg-require-effective-target vect_int } */

#include <stdarg.h>
#include "tree-vect.h"

#define A 3
#define B 4
#define N 256

short src[N], dst[N];

void foo (short * __restrict__ dst, short * __restrict__ src, int h, int stride, int dummy)
{
  int i;
  h /= 16;
  for (i = 0; i < h; i++)
    {
      dst[0] += A*src[0] + src[stride];
      dst[1] += A*src[1] + src[1+stride];
      dst[2] += A*src[2] + src[2+stride];
      dst[3] += A*src[3] + src[3+stride];
      dst[4] += A*src[4] + src[4+stride];
      dst[5] += A*src[5] + src[5+stride];
      dst[6] += A*src[6] + src[6+stride];
      dst[7] += A*src[7] + src[7+stride];
      dst += 8;
      src += 8;
      if (dummy == 32)
        abort ();
   }
}


int main (void)
{
  int i;

  check_vect ();

  for (i = 0; i < N; i++)
    {
       dst[i] = 0;
       src[i] = i;
    }

  foo (dst, src, N, 8, 0);

  for (i = 0; i < N/2; i++)
    {
      if (dst[i] != A * i + i + 8)
        abort ();
    }

  return 0;
}

/* Exclude POWER8 (only POWER cpu for which vect_element_align is true)
   because loops have vectorized before SLP gets a shot.  */
/* { dg-final { scan-tree-dump-times "basic block vectorized" 1 "slp1" { target { vect_element_align && { ! powerpc*-*-* } } } } } */

