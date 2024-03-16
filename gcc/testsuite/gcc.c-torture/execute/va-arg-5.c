#include <stdarg.h>

void abort (void);
void exit (int);

void
va_double (int n, ...)
{
  va_list args;

  va_start (args, n);

  if (va_arg (args, double) != 3.141592)
    abort ();
  if (va_arg (args, double) != 2.71827)
    abort ();
  if (va_arg (args, double) != 2.2360679)
    abort ();
  if (va_arg (args, double) != 2.1474836)
    abort ();

  va_end (args);
}

void
va_long_double (int n, ...)
{
  va_list args;

  va_start (args, n);

  if (va_arg (args, long double) != 3.141592L)
    abort ();
  if (va_arg (args, long double) != 2.71827L)
    abort ();
  if (va_arg (args, long double) != 2.2360679L)
    abort ();
  if (va_arg (args, long double) != 2.1474836L)
    abort ();

  va_end (args);
}

int
main (void)
{
  va_double (4, 3.141592, 2.71827, 2.2360679, 2.1474836);
  va_long_double (4, 3.141592L, 2.71827L, 2.2360679L, 2.1474836L);
  exit (0);
}
