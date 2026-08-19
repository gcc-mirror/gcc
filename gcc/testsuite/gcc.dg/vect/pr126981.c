/* PR tree-optimization/126981 */
/* { dg-require-effective-target vect_int } */

#include "tree-vect.h"

#define N 64

/* Clip to [0, 65535].  At INT_MIN the negation is its own inverse, so the
   shift yields -1 and the result is 65535 rather than 0.  */

__attribute__ ((noipa)) void
clip (unsigned short *__restrict out, const int *__restrict in, int n)
{
  for (int i = 0; i < n; ++i)
    {
      int x = in[i];
      out[i] = ((unsigned int) x > 65535u
		? (int) (-(unsigned int) x) >> 31
		: x);
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
      unsigned short ref = ((unsigned int) x > 65535u
			    ? (int) (-(unsigned int) x) >> 31
			    : x);
      if (out[i] != ref)
	abort ();
    }

  return 0;
}
