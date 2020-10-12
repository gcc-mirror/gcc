/* { dg-require-effective-target vect_int } */

#include <stdarg.h>
#include "tree-vect.h"

#define A 3
#define B 4
#define N 256

short src[N], dst[N];

void foo (short * __restrict__ dst, short * __restrict__ src, int h,
	  int stride)
{
  int i;
  h /= 16;
  for (i = 0; i < h; i++)
    {
      dst[0] = A*src[0] + B*src[1];
      dst[1] = A*src[1] + B*src[2];
      dst[2] = A*src[2] + B*src[3];
      dst[3] = A*src[3] + B*src[4];
      dst[4] = A*src[4] + B*src[5];
      dst[5] = A*src[5] + B*src[6];
      dst[6] = A*src[6] + B*src[7];
      dst[7] = A*src[7] + B*src[8];
      dst += stride;
      src += stride;
      asm volatile ("" ::: "memory");
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

  foo (dst, src, N, 8);

  for (i = 0; i < N/2; i++)
    {
      if (dst[i] != A * src[i] + B * src[i+1])
        abort ();
    }

  return 0;
}

/* Exclude POWER8 (only POWER cpu for which vect_element_align is true)
   because loops have vectorized before SLP gets a shot.  */
/* { dg-final { scan-tree-dump-times "optimized: basic block" 1 "slp1"  { target { { vect_int_mult && vect_element_align } && { ! powerpc*-*-* } } } } } */

