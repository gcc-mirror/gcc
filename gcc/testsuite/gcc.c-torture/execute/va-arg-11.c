/* Test va_arg when the result is ignored and only the pointer increment
   side effect is used.  */
#include <stdarg.h>

static int
foo (int a, ...)
{
  va_list va;
  int i, res;

  va_start (va, a);

  for (i = 0; i < 4; ++i)
    (void) va_arg (va, int);

  res = va_arg (va, int);

  va_end (va);

  return res;
}

int
main (void)
{
  if (foo (5, 4, 3, 2, 1, 0))
    abort ();
  exit (0);
}
