/* { dg-skip-if "requires io" { freestanding } }  */
/* { dg-xfail-run-if {unexpected PTX 'vprintf' return value} { nvptx-*-* } } */

#ifndef test
#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>

volatile int should_optimize;

int
__attribute__((noinline))
__vprintf_chk (int flag, const char *fmt, va_list ap)
{
#ifdef __OPTIMIZE__
  if (should_optimize)
    abort ();
#endif
  should_optimize = 1;
  return vprintf (fmt, ap);
}

void
inner (int x, ...)
{
  va_list ap, ap2;
  va_start (ap, x);
  va_start (ap2, x);

  switch (x)
    {
#define test(n, ret, opt, fmt, args) \
    case n:					\
      should_optimize = opt;			\
      __vprintf_chk (1, fmt, ap);		\
      if (! should_optimize)			\
	abort ();				\
      should_optimize = 0;			\
      if (__vprintf_chk (1, fmt, ap2) != ret)	\
	abort ();				\
      if (! should_optimize)			\
	abort ();				\
      break;
#include "vprintf-chk-1.c"
#undef test
    default:
      abort ();
    }

  va_end (ap);
  va_end (ap2);
}

int
main (void)
{
#define test(n, ret, opt, fmt, args) \
  inner args;
#include "vprintf-chk-1.c"
#undef test
  return 0;
}

#else
  test (0, 5, 0, "hello", (0));
  test (1, 6, 1, "hello\n", (1));
  test (2, 1, 1, "a", (2));
  test (3, 0, 1, "", (3));
  test (4, 5, 0, "%s", (4, "hello"));
  test (5, 6, 0, "%s", (5, "hello\n"));
  test (6, 1, 0, "%s", (6, "a"));
  test (7, 0, 0, "%s", (7, ""));
  test (8, 1, 0, "%c", (8, 'x'));
  test (9, 7, 0, "%s\n", (9, "hello\n"));
  test (10, 2, 0, "%d\n", (10, 0));
#endif
