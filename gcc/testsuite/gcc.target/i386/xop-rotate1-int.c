/* PR target/49411 */
/* { dg-do run } */
/* { dg-require-effective-target xop } */
/* { dg-options "-O2 -mxop" } */

#include "xop-check.h"

#include <x86intrin.h>

extern void abort (void);

union
{
  __m128i v;
  unsigned char c[16];
  unsigned short s[8];
  unsigned int i[4];
  unsigned long long l[2];
} a, b, c, d;

#define TEST1(F, N, S, SS) \
do {							\
  for (i = 0; i < sizeof (a.F) / sizeof (a.F[0]); i++)	\
    a.F[i] = i * 17;					\
  s = _mm_set1_epi##SS (N);				\
  b.v = _mm_roti_epi##S (a.v, N);			\
  c.v = _mm_rot_epi##S (a.v, s);			\
  for (i = 0; i < sizeof (a.F) / sizeof (a.F[0]); i++)	\
    {							\
      int mask = __CHAR_BIT__ * sizeof (a.F[i]) - 1;	\
      d.F[i] = a.F[i] << (N & mask);			\
      if (N & mask)					\
	d.F[i] |= a.F[i] >> (mask + 1 - (N & mask));	\
      if (b.F[i] != c.F[i] || b.F[i] != d.F[i])		\
	abort ();					\
    }							\
} while (0)
#define TEST(N) \
  TEST1 (c, N, 8, 8);					\
  TEST1 (s, N, 16, 16);					\
  TEST1 (i, N, 32, 32);					\
  TEST1 (l, N, 64, 64x)

volatile int n;

static void
xop_test (void)
{
  unsigned int i;
  __m128i s;

#ifndef NON_CONST
  TEST (5);
  TEST (-5);
  TEST (0);
  TEST (31);
#else
  n = 5; TEST (n);
  n = -5; TEST (n);
  n = 0; TEST (n);
  n = 31; TEST (n);
#endif
}
