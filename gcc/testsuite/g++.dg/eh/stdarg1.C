// PR c++/72809
// { dg-do compile }

#include <stdarg.h>

int
foo (int a, ...)
{
  va_list ap;
  int r = 0;
  va_start (ap, a);
  try
    {
      if (a == 1)
	throw (ap);
    }
  catch (va_list b)
    {
      r = va_arg (b, int);
    }
  va_end (ap);
  return r;
}

int
main ()
{
  if (foo (0) != 0 || foo (1, 7) != 7)
    __builtin_abort ();
}
