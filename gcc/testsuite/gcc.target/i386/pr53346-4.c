/* { dg-do run } */
/* { dg-options "-O2 -msse2" } */
/* { dg-require-effective-target sse2 } */

#include "sse2-check.h"
#include "pr53346-3.c"

static void
sse2_test ()
{
  v4sf a = __extension__(v4sf) { 0, 1, 2, 3 };
  v4sf b = __extension__(v4sf) { 4, 5, 6, 7 };
  v4sf exp = __extension__(v4sf) { 1, 2, 5, 3 };
  v4sf dest;
  dest = foo (a, b);
  if (__builtin_memcmp (&dest, &exp, 16))
    __builtin_abort ();

  exp = __extension__ (v4sf) { 1, 5, 2, 3 };
  dest = foo1 (a, b);
  if (__builtin_memcmp (&dest, &exp, 16))
    __builtin_abort ();

  exp = __extension__ (v4sf) { 1, 2, 3, 5 };
  dest = foo2 (a, b);
  if (__builtin_memcmp (&dest, &exp, 16))
    __builtin_abort ();

  exp = __extension__ (v4sf) { 1, 4, 5, 6 };
  dest = foo3 (a, b);
  if (__builtin_memcmp (&dest, &exp, 16))
    __builtin_abort ();

  exp = __extension__ (v4sf) { 3, 6, 7, 5 };
  dest = foo4 (a, b);
  if (__builtin_memcmp (&dest, &exp, 16))
    __builtin_abort ();

  exp = __extension__ (v4sf) { 2, 4, 7, 6 };
  dest = foo5 (a, b);
  if (__builtin_memcmp (&dest, &exp, 16))
    __builtin_abort ();

  exp = __extension__ (v4sf) { 2, 4, 3, 6 };
  dest = foo6 (a, b);
  if (__builtin_memcmp (&dest, &exp, 16))
    __builtin_abort ();

  exp = __extension__ (v4sf) { 2, 3, 4, 6 };
  dest = foo7 (a, b);
  if (__builtin_memcmp (&dest, &exp, 16))
    __builtin_abort ();

  exp = __extension__ (v4sf) { 2, 4, 6, 3 };
  dest = foo8 (a, b);
  if (__builtin_memcmp (&dest, &exp, 16))
    __builtin_abort ();

}
