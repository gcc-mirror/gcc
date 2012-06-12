/* PR middle-end/37009 */
/* { dg-do run { target { { i?86-*-* x86_64-*-* } && ilp32 } } } */
/* { dg-additional-options "-mmmx -msse2" } */
/* { dg-require-effective-target sse2_runtime } */

#include <stdarg.h>
#include <emmintrin.h>
#include "check.h"

#ifndef ALIGNMENT
#define ALIGNMENT	16
#endif

typedef int aligned __attribute__((aligned(ALIGNMENT)));

void
bar (char *p, int size)
{
  __builtin_strncpy (p, "good", size);
}

__m128 a = { 1.0 };

void
test (va_list arg)
{
  char *p;
  aligned i;
  int size;
  double x;
  __m128 e;

  size = va_arg (arg, int);
  if (size != 5)
    abort ();

  p = __builtin_alloca (size + 1);

  x = va_arg (arg, double);
  if (x != 5.0)
    abort ();

  bar (p, size);
  if (__builtin_strncmp (p, "good", size) != 0)
    {
#ifdef DEBUG
      p[size] = '\0';
      printf ("Failed: %s != good\n", p);
#endif
      abort ();
    }

  if (check_int (&i,  __alignof__(i)) != i)
    abort ();

  e = va_arg (arg, __m128);
  if (__builtin_memcmp (&e, &a, sizeof (e)))
    abort ();
}

void
foo (const char *fmt, ...)
{
  va_list arg;
  va_start (arg, fmt);
  test (arg);
  va_end (arg);
}

int
main (void)
{
  __m128 x = { 1.0 };

  foo ("foo", 5, 5.0, x);

  return 0;
}
