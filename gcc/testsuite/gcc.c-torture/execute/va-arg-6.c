#include <stdarg.h>

void abort (void);
void exit (int);

void
f (int n, ...)
{
  va_list args;

  va_start (args, n);

  if (va_arg (args, int) != 10)
    abort ();
  if (va_arg (args, long long) != 10000000000LL)
    abort ();
  if (va_arg (args, int) != 11)
    abort ();
  if (va_arg (args, long double) != 3.14L)
    abort ();
  if (va_arg (args, int) != 12)
    abort ();
  if (va_arg (args, int) != 13)
    abort ();
  if (va_arg (args, long long) != 20000000000LL)
    abort ();
  if (va_arg (args, int) != 14)
    abort ();
  if (va_arg (args, double) != 2.72)
    abort ();

  va_end(args);
}

int
main (void)
{
  f (4, 10, 10000000000LL, 11, 3.14L, 12, 13, 20000000000LL, 14, 2.72);
  exit (0);
}
