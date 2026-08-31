/* PR tree-optimization/127166 */
/* { dg-require-effective-target int32 } */
/* { dg-require-effective-target vect_int } */

#include "tree-vect.h"

#define N 64

__attribute__ ((noipa)) void
clip (unsigned short *__restrict out, const int *__restrict in, int n)
{
  for (int i = 0; i < n; ++i)
    {
      int x = in[i] & __INT_MAX__;
      out[i] = ((unsigned int) x > 65535u
		? (unsigned int) (-x) >> 31 : x);
    }
}

int
main (void)
{
  int in[N];
  unsigned short out[N];

  check_vect ();

  for (int i = 0; i < N; ++i)
    in[i] = (i & 1) ? -1 : 70000 + i;

  clip (out, in, N);

#pragma GCC novector
  for (int i = 0; i < N; ++i)
    {
      if (out[i] != 1)
	abort ();
    }

  return 0;
}

/* The unsigned right shift does not produce the all-ones clip value.  */
/* { dg-final { scan-tree-dump-not "\\.SAT_TRUNC" "vect" } } */
