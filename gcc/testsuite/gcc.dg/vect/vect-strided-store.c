/* { dg-require-effective-target vect_float } */

#include <stdarg.h>
#include "tree-vect.h"

void __attribute__((noinline))
sumit (float * __restrict dest,
       float * __restrict src, float * __restrict src2,
       int stride, int n)
{
  int i;
  for (i = 0; i < n; i++)
    dest[i*stride] = src[i] + src2[i];
}

int main()
{
  int i, stride;
  float src[] = {1, 2, 3, 4, 5, 6, 7, 8};
  float dest[64];
  check_vect ();
  for (stride = 0; stride < 8; stride++)
    {
      sumit (dest, src, src, stride, 8);
      if (!stride && dest[0] != 16)
	abort();
      else if (stride)
	for (i = 0; i < 8; i++)
	  if (2*src[i] != dest[i*stride])
	    abort ();
    }
  return 0;
}

/* { dg-final { scan-tree-dump-times "vectorized 1 loops" 1 "vect" } } */
