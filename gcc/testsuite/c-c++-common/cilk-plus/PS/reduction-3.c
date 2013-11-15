/* { dg-do run } */
/* { dg-options "-O3 -fcilkplus" } */

/* FIXME: This test has been xfailed until reductions are fixed.  */

#define N 256
#if HAVE_IO
#include <stdio.h>
#endif
#include <malloc.h>

int
reduction_simd (int *a)
{
  int s = 0;

#pragma simd reduction (+:s)
  for (int i = 0; i < N; i++)
    {
      s += a[i];
    }

  return s;
}

int
main ()
{
  int *a = (int *) malloc (N * sizeof (int));
  int i, s = (N - 1) * N / 2;

  for (i = 0; i < N; i++)
    {
      a[i] = i;
    }
#if HAVE_IO
  printf ("%d, %d\n", s, reduction_simd (a));
#endif
  if (s == reduction_simd (a))
    return 0;
  else
    return 1;
}
