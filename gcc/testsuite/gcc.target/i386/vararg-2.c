/* PR middle-end/36859 */
/* { dg-do run } */
/* { dg-options "-w" { target { lp64 } } } */
/* { dg-options "-w -msse2 -mpreferred-stack-boundary=2" { target { ilp32 } } } */
/* { dg-require-effective-target sse2 } */

#include "sse2-check.h"
#include <stdarg.h>
#include <emmintrin.h>

__m128
__attribute__((noinline))
test (int a, ...)
{
  __m128 x;
  va_list va_arglist;

  va_start (va_arglist, a);
  x = va_arg (va_arglist, __m128);
  va_end (va_arglist);
  return x;
}

__m128 n1 = { -283.3, -23.3, 213.4, 1119.03 };

int
__attribute__((noinline))
foo (void)
{
  __m128 x = test (1, n1);
  if (__builtin_memcmp (&x, &n1, sizeof (x)) != 0)
    abort ();
  return 0;
}

static void
__attribute__((noinline))
sse2_test (void)
{
  foo ();
}
