// P3348R4 - C++26 should refer to C23 not C17
// { dg-do run { target c++26 } }

#include <stdarg.h>

int
main ()
{
  int v = 0;
  auto a = [&] (...) {
    va_list ap;
    va_start (ap);
    int b = 42;
    if (v)
      b = va_arg (ap, int);
    va_end (ap);
    return b;
  };
  if (a () != 42)
    __builtin_abort ();
  v = 1;
  if (a (1, 2) != 1)
    __builtin_abort ();
  if (a (13, 2.0f, 2ULL) != 13)
    __builtin_abort ();
}
