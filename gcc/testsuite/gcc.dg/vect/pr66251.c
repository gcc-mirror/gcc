/* { dg-require-effective-target vect_int } */
/* { dg-require-effective-target vect_double } */
/* { dg-require-effective-target vect_doubleint_cvt } */
/* { dg-require-effective-target vect_intdouble_cvt } */
/* { dg-require-effective-target vect_pack_trunc } */
/* { dg-require-effective-target vect_unpack } */
/* { dg-require-effective-target vect_hw_misalign } */

#include "tree-vect.h"

void __attribute__((noinline,noclone))
test1(_Complex double *a, _Complex int *b, int stride, int n)
{
  int i;
  for (i = 0; i < n; i++)
    {
      a[i*stride] = b[i*stride];
    }
}

void __attribute__((noinline,noclone))
test2(_Complex int *a, _Complex double *b, int stride, int n)
{
  int i;
  for (i = 0; i < n; i++)
    {
      a[i*stride] = b[i*stride];
    }
}

_Complex int ia[256];
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
	  __asm__ volatile ("");
	}

      test1(da, ia, stride, 256/stride);

      for (i = 0; i < 256/stride; i++)
	{
	  if (da[i*stride] != ia[i*stride])
	    abort ();
	}

      for (i = 0; i < 256; i++)
	{
	  __real__ da[i] = (i + stride + 1) % 29;
	  __imag__ da[i] = (i + stride + 1) % 31;
	  __asm__ volatile ("");
	}

      test2(ia, da, stride, 256/stride);

      for (i = 0; i < 256/stride; i++)
	{
	  if (da[i*stride] != ia[i*stride])
	    abort ();
	}
    }
  return 0;
}

/* { dg-final { scan-tree-dump-times "vectorized 1 loops" 2 "vect" } } */
