/* { dg-require-effective-target vect_double } */
/* { dg-require-effective-target vect_hw_misalign } */

#include "tree-vect.h"

void __attribute__((noinline,noclone))
test1(_Complex double * __restrict__ a, _Complex double * __restrict__ b,
      double * __restrict__ c, int stride, int n)
{
  int i;
  for (i = 0; i < n; i++)
    {
      a[i*stride] = 0.5 * b[i*stride] * c[i*stride];
    }
}

double ca[256];
_Complex double ia[256];
_Complex double da[256];

extern void abort (void);

int main ()
{
  int i;
  int stride;

  check_vect ();

  for (stride = 1; stride < 15; stride++)
    {
      for (i = 0; i < 256; i++)
	{
	  __real__ ia[i] = (i + stride) % 19;
	  __imag__ ia[i] = (i + stride) % 23;
	  ca[i] = (i + stride) % 29;
	  __asm__ volatile ("");
	}

      test1(da, ia, ca, stride, 256/stride);

#pragma GCC novector
      for (i = 0; i < 256/stride; i++)
	{
	  if (da[i*stride] != 0.5 * ia[i*stride] * ca[i*stride])
	    abort ();
	}
    }
  return 0;
}

/* { dg-final { scan-tree-dump-times "vectorized 1 loops" 1 "vect" } } */
