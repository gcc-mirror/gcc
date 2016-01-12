/* { dg-require-effective-target vect_condition } */
/* { dg-require-effective-target vect_simd_clones } */
/* { dg-additional-options "-fopenmp-simd" } */

#include "tree-vect.h"

#define N 64
int a[N];
int c[N];

__attribute__ ((noinline)) int
foo (void)
{
  int i, res = 0;
#pragma omp simd safelen(8)
  for (i = 0; i < N; i++)
  {
    int t = a[i];
    if (c[i] != 0)
      if (t != 100 & t > 5)
	res += 1;
  }
  return res;
}

__attribute__ ((noinline)) int
hundred (void)
{
  return 100;
}


int main (void)
{
  int i;

  check_vect ();

  for (i = 0; i < N; i++)
    {
      c[i] = i & 1;
      switch (i & 3)
	{
case 0:
	  a[i] = hundred ();
	  break;
case 1:
	  a[i] = 1;
	  break;
default:
	  a[i] = i + 6;
	  break;
	}
    }
  if (foo () != 16)
    abort ();
  return 0;
}

/* { dg-final { scan-tree-dump-times "vectorized 1 loops" 1 "vect" } } */
