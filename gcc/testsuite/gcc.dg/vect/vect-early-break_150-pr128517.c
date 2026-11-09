/* { dg-add-options vect_early_break } */
/* { dg-require-effective-target vect_early_break_hw } */
/* { dg-require-effective-target vect_int } */
/* { dg-additional-options "-O3" } */
/* { dg-additional-options "-mavx2" { target avx2_runtime } } */

#include "tree-vect.h"

unsigned calls;

static unsigned rec(unsigned d, unsigned x)
{
  calls++;
  if (d == 0)
    return 0;
  rec(d - 1, (x ? 2 : calls) % (2 * d + 1));
  return calls % 10 / (calls / (x | 1) | 1);
}

int main ()
{
  check_vect ();

  /* Odd D exercises the scalar loop reached when the niter assumption
     check fails, even D the scalar epilogue of the vectorized loop.  */
  for (volatile unsigned d = 7; d <= 8; d++)
    {
      calls = 0;
      if (rec(d, 1) != (d & 1 ? 0 : 1) || calls != d + 1)
	__builtin_abort ();
    }
  return 0;
}
