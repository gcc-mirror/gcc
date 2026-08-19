/* PR tree-optimization/126982 */
/* { dg-require-effective-target vect_int } */

#include "tree-vect.h"

#define N 64

/* A variable shift keeps the shift result in int, so the comparison of the
   saturating truncation runs at a narrower precision than the value being
   truncated.  The comparison then only looks at the low bits, and the
   truncation is not a saturating one.  */

__attribute__ ((noipa)) void
sat_trunc (signed char *__restrict out, const short *__restrict in,
	   const unsigned short *__restrict shifts, int n)
{
  for (int i = 0; i < n; ++i)
    {
      short x = in[i] >> (shifts[i] & 15);
      signed char t = (signed char) x;
      out[i] = (-128 <= x && x <= 127 ? t : x < 0 ? -128 : 127);
    }
}

int
main (void)
{
  short in[N];
  unsigned short shifts[N];
  signed char out[N];

  check_vect ();

  for (int i = 0; i < N; ++i)
    {
      in[i] = (short) (i * 7919 - 32768);
      shifts[i] = i % 16;
    }

  sat_trunc (out, in, shifts, N);

#pragma GCC novector
  for (int i = 0; i < N; ++i)
    {
      short x = in[i] >> (shifts[i] & 15);
      signed char t = (signed char) x;
      signed char ref = (-128 <= x && x <= 127 ? t : x < 0 ? -128 : 127);
      if (out[i] != ref)
	abort ();
    }

  return 0;
}
