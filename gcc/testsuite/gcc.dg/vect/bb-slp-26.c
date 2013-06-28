/* { dg-require-effective-target vect_int } */

#include <stdarg.h>
#include "tree-vect.h"

#define A 3
#define B 4
#define N 256

char src[N], dst[N];

void foo (char * __restrict__ dst, char * __restrict__ src, int h,
          int stride, int dummy)
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
       src[i] = i/8;
    }

  foo (dst, src, N, 8, 0);

  for (i = 0; i < N/2; i++)
    {
      if (dst[i] != A * src[i] + src[i+8])
        abort ();
    }

  return 0;
}

/* { dg-final { scan-tree-dump-times "Vectorized basic-block" 1 "slp" { target vect64 } } } */
/* { dg-final { cleanup-tree-dump "slp" } } */

