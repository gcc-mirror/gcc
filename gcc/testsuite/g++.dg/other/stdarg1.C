// Test stdarg function with anonymous argument
// { dg-do run }

#include <stdarg.h>

extern "C" void abort (void);

void baz (va_list list)
{
  if (va_arg (list, long) != 3)
    abort ();
}

void foo (long p1, long, long p2, ...)
{
  va_list list;
  va_start (list, p2);
  baz (list);
  va_end (list);
}

int main ()
{
  foo (0, 1, 2, 3);
  return 0;
}
