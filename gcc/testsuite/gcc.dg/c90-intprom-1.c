/* Test for integer promotion rules: C90 subset of types.  */
/* Origin: Joseph Myers <jsm@polyomino.org.uk> */
/* { dg-do compile } */
/* { dg-options "-std=iso9899:1990 -pedantic-errors" } */

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
  /* One type is unsigned long.  */
  CHECK(unsigned long, unsigned long, unsigned long);
  CHECK(unsigned int, unsigned long, unsigned long);
  CHECK(unsigned long, unsigned int, unsigned long);
  CHECK(int, unsigned long, unsigned long);
  CHECK(long, unsigned long, unsigned long);
  CHECK(unsigned long, int, unsigned long);
  CHECK(unsigned long, long, unsigned long);
  /* long and unsigned int.  */
#if LONG_MAX >= UINT_MAX
  CHECK(unsigned int, long, long);
  CHECK(long, unsigned int, long);
#else
  CHECK(unsigned int, long, unsigned long);
  CHECK(long, unsigned int, unsigned long);
#endif
  /* One type is long.  */
  CHECK(long, long, long);
  CHECK(int, long, long);
  CHECK(long, int, long);
  /* One type is unsigned int.  */
  CHECK(unsigned int, unsigned int, unsigned int);
  CHECK(int, unsigned int, unsigned int);
  CHECK(unsigned int, int, unsigned int);
  /* Otherwise int.  */
  CHECK(int, int, int);
}
