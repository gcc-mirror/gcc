/* { dg-do run } */
/* { dg-require-effective-target lp64 } */
/* { dg-require-effective-target sse4 } */
/* { dg-options "-O2 -msse4.1" } */

#ifndef CHECK_H
#define CHECK_H "sse4_1-check.h"
#endif

#ifndef TEST
#define TEST sse4_1_test
#endif

#include CHECK_H

#include <smmintrin.h>
#include <string.h>

typedef long T __attribute__((may_alias));
struct S { __m128i d; };

__m128i
__attribute__((noinline))
foo (__m128i y, long x)
{
  struct S s;

  s.d = y;
  ((T *) &s.d)[1] = x;
  return s.d;
}

static void
TEST (void)
{
  union
    {
      __m128i x;
      unsigned int i[4];
      unsigned long l[2];
    } res, val, tmp;
  unsigned long ins[4] = { 3, 4, 5, 6 };

  val.i[0] = 0x35251505;
  val.i[1] = 0x75655545;
  val.i[2] = 0xB5A59585;
  val.i[3] = 0xF5E5D5C5;

  res.x = foo (val.x, ins[3]);

  tmp.x = val.x;
  tmp.l[1] = ins[3];
  if (memcmp (&tmp, &res, sizeof (tmp)))
    abort ();
}
