#ifndef test
#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>

void
inner (int x, ...)
{
  va_list ap, ap2;
  va_start (ap, x);
  va_start (ap2, x);

  switch (x)
    {
#define test(n, ret, fmt, args) \
    case n:				\
      vprintf (fmt, ap);		\
      if (vprintf (fmt, ap2) != ret)	\
	abort ();			\
      break;
#include "vprintf-1.c"
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
#define test(n, ret, fmt, args) \
  inner args;
#include "vprintf-1.c"
#undef test
  return 0;
}

#else
  test (0, 5, "hello", (0));
  test (1, 6, "hello\n", (1));
  test (2, 1, "a", (2));
  test (3, 0, "", (3));
  test (4, 5, "%s", (4, "hello"));
  test (5, 6, "%s", (5, "hello\n"));
  test (6, 1, "%s", (6, "a"));
  test (7, 0, "%s", (7, ""));
  test (8, 1, "%c", (8, 'x'));
  test (9, 7, "%s\n", (9, "hello\n"));
  test (10, 2, "%d\n", (10, 0));
#endif
