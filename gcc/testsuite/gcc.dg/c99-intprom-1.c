/* Test for integer promotion rules: extended to long long by C99.  */
/* Origin: Joseph Myers <jsm@polyomino.org.uk> */
/* { dg-do compile } */
/* { dg-options "-std=iso9899:1999 -pedantic-errors" } */

#include <limits.h>

#define CHECK(T1, T2, TC)			\
  do {						\
    T1 a = 0;					\
    T2 b = 0;					\
    TC *c = 0;					\
    __typeof__(a+b) *d = 0;			\
    c = d;					\
    d = c;					\
  } while (0)

void
f (void)
{
  /* Same type.  */
  CHECK(int, int, int);
  CHECK(unsigned int, unsigned int, unsigned int);
  CHECK(long, long, long);
  CHECK(unsigned long, unsigned long, unsigned long);
  CHECK(long long, long long, long long);
  CHECK(unsigned long long, unsigned long long, unsigned long long);
  /* Both signed.  */
  CHECK(int, long, long);
  CHECK(int, long long, long long);
  CHECK(long, int, long);
  CHECK(long, long long, long long);
  CHECK(long long, int, long long);
  CHECK(long long, long, long long);
  /* Both unsigned.  */
  CHECK(unsigned int, unsigned long, unsigned long);
  CHECK(unsigned int, unsigned long long, unsigned long long);
  CHECK(unsigned long, unsigned int, unsigned long);
  CHECK(unsigned long, unsigned long long, unsigned long long);
  CHECK(unsigned long long, unsigned int, unsigned long long);
  CHECK(unsigned long long, unsigned long, unsigned long long);
  /* Unsigned of greater or equal rank.  */
  CHECK(int, unsigned int, unsigned int);
  CHECK(int, unsigned long, unsigned long);
  CHECK(int, unsigned long long, unsigned long long);
  CHECK(unsigned int, int, unsigned int);
  CHECK(long, unsigned long, unsigned long);
  CHECK(long, unsigned long long, unsigned long long);
  CHECK(unsigned long, int, unsigned long);
  CHECK(unsigned long, long, unsigned long);
  CHECK(long long, unsigned long long, unsigned long long);
  CHECK(unsigned long long, int, unsigned long long);
  CHECK(unsigned long long, long, unsigned long long);
  CHECK(unsigned long long, long long, unsigned long long);
  /* Signed of greater rank.  */
#if LONG_MAX >= UINT_MAX
  CHECK(unsigned int, long, long);
  CHECK(long, unsigned int, long);
#else
  CHECK(unsigned int, long, unsigned long);
  CHECK(long, unsigned int, unsigned long);
#endif
#if LLONG_MAX >= UINT_MAX
  CHECK(unsigned int, long long, long long);
  CHECK(long long, unsigned int, long long);
#else
  CHECK(unsigned int, long long, unsigned long long);
  CHECK(long long, unsigned int, unsigned long long);
#endif
#if LLONG_MAX >= ULONG_MAX
  CHECK(unsigned long, long long, long long);
  CHECK(long long, unsigned long, long long);
#else
  CHECK(unsigned long, long long, unsigned long long);
  CHECK(long long, unsigned long, unsigned long long);
#endif
}
