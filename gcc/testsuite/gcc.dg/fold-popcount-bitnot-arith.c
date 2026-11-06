/* { dg-do run } */
/* { dg-options "-O2" } */

/* Correctness test: popcount(~X) == PREC - popcount(X).
   Checks unsigned int, signed int, and unsigned long long, including
   boundary cases with all-zeros and all-ones.  */

#include <stdlib.h>
#include <limits.h>

int f_uint (unsigned int x)       { return __builtin_popcount   (~x); }
int f_int  (int x)                { return __builtin_popcount   (~x); }
int f_ull  (unsigned long long x) { return __builtin_popcountll (~x); }

#define PREC_UINT ((int)(sizeof (unsigned int)       * CHAR_BIT))
#define PREC_ULL  ((int)(sizeof (unsigned long long) * CHAR_BIT))

int
main (void)
{
  /* unsigned int test vectors.  */
  static const unsigned int uv[] = {
    0U,
    1U,
    0x12345678U,
    0x07FFFFFFU,   /* popcount = 27, popcount(~) = 5  */
    0xFFFFFFFFU,   /* all-ones: popcount(~) = 0       */
  };
  for (unsigned i = 0; i < sizeof uv / sizeof *uv; i++)
    {
      int expected = PREC_UINT - __builtin_popcount (uv[i]);
      if (f_uint (uv[i]) != expected)
	abort ();
    }

  /* signed int test vectors, including negative values.  */
  static const int iv[] = {
    0,
    1,
    -1,
    0x12345678,
    0x07FFFFFF,
  };
  for (unsigned i = 0; i < sizeof iv / sizeof *iv; i++)
    {
      int expected = PREC_UINT
		     - __builtin_popcount ((unsigned int) iv[i]);
      if (f_int (iv[i]) != expected)
	abort ();
    }

  /* unsigned long long test vectors.  */
  static const unsigned long long ullv[] = {
    0ULL,
    1ULL,
    0x123456789ABCDEF0ULL,
    0x3FFFFFFFFFFFFFFFULL,  /* 2 bits clear: popcount(~) = 2  */
    ~0ULL,                  /* all-ones: popcount(~) = 0      */
  };
  for (unsigned i = 0; i < sizeof ullv / sizeof *ullv; i++)
    {
      int expected = PREC_ULL - __builtin_popcountll (ullv[i]);
      if (f_ull (ullv[i]) != expected)
	abort ();
    }
  return 0;
}
