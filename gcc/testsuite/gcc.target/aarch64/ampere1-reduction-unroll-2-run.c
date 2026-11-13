/* { dg-do run } */
/* { dg-require-effective-target arm_v8_2a_dotprod_neon_hw } */
/* { dg-options "-O3 -march=armv8.2-a+dotprod -mtune=ampere1" } */

/* Check the kernels against scalar loops for trip counts and offsets that
   exercise the unrolled loop and its epilogues.  */

#include "ampere1-reduction-unroll-2.c"

#define N 4099

signed char sa[N], sb[N];
unsigned char ua[N], ub[N];
_Bool flags[N];

#define CHECK(TYPE, CALL, EXPR)			\
  {						\
    TYPE ref = 0;				\
    _Pragma ("GCC novector")			\
    for (int i = 0; i < n; i++)			\
      ref += EXPR;				\
    if (CALL != ref)				\
      __builtin_abort ();			\
  }

int
main (void)
{
  for (int i = 0; i < N; i++)
    {
      sa[i] = i * 37 + 11;
      sb[i] = i * 101 - 7;
      ua[i] = i * 59 + 3;
      ub[i] = i * 13 + 200;
      flags[i] = (i * 7) % 3 == 0;
    }

  static const int lens[] = { 0, 1, 15, 16, 17, 63, 64, 65, 127, 128, 129,
			      255, 256, 257, 1000, 4096, N - 1 };
  for (int off = 0; off < 2; off++)
    for (unsigned int l = 0; l < sizeof (lens) / sizeof (lens[0]); l++)
      {
	int n = lens[l] - off;
	if (n < 0)
	  continue;
	CHECK (int, dot (sa + off, sb, n), sa[off + i] * sb[i]);
	CHECK (int, sad (ua + off, ub, n), __builtin_abs (ua[off + i] - ub[i]));
	CHECK (int, sad_nodot (ua + off, ub, n),
	       __builtin_abs (ua[off + i] - ub[i]));
	CHECK (int, sum (ua + off, n), ua[off + i]);
	CHECK (unsigned short, sum16 (ua + off, n), ua[off + i]);
	CHECK (int, count (flags + off, n), flags[off + i]);
      }
  return 0;
}
