/* { dg-do run } */
/* { dg-options "-O2 -ftree-vectorize -mavx512bw" } */
/* { dg-require-effective-target avx512bw } */

#define AVX512BW
#include "avx512f-helper.h"

extern void abort (void);

void __attribute__((noinline,noclone))
test1 (int *a, int *b, int *c)
{
  int i;
  for (i = 0; i < 16; i++)
    {
      if ((i == 0) || (i == 3))
	a[i] = b[i];
      else
	a[i] = c[i];
    }
}

void
TEST ()
{
  int a[16], b[16], c[16], i;

  for (i = 0; i < 16; i++)
    {
      a[i] = i;
      b[i] = -i;
    }

  test1 (a, b, c);

  for (i = 0; i < 16; i++)
    {
      if ((i == 0) || (i == 3))
	{
	  if (a[i] != b[i])
	    abort ();
	}
      else
	{
	  if (a[i] != c[i])
	    abort ();
	}
    }
}
