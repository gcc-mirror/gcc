#include <stdarg.h>

main ()
{
  static double f (int x, ...)
    {
      va_list args;
      double a;

      va_start (args, x);
      a = va_arg (args, double);
      va_end (args);
      return a;
    }

  if (f (1, (double)1) != 1.0)
    abort ();
  exit (0);
}
