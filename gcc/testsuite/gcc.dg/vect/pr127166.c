/* PR tree-optimization/127166 */
/* { dg-additional-options "-fwrapv" } */
/* { dg-require-effective-target int32 } */
/* { dg-require-effective-target vect_int } */

#include "tree-vect.h"

#define N 64

__attribute__ ((noipa)) void
clip (unsigned short *__restrict out, const int *__restrict in, int n)
{
  for (int i = 0; i < n; ++i)
    {
      int x = in[i];
      out[i] = ((unsigned int) x > 65535u ? (-x) >> 31 : x);
    }
}

int
main (void)
{
  int in[N];
  unsigned short out[N];

  check_vect ();

  for (int i = 0; i < N; ++i)
    in[i] = (i & 3) == 0 ? (-__INT_MAX__ - 1) : i * 12345 - 30000;

  clip (out, in, N);

#pragma GCC novector
  for (int i = 0; i < N; ++i)
    {
      int x = in[i];
      unsigned short ref;

      if (x == (-__INT_MAX__ - 1) || x > 65535)
	ref = 65535;
      else if (x < 0)
	ref = 0;
      else
	ref = x;
      if (out[i] != ref)
	abort ();
    }

  return 0;
}
