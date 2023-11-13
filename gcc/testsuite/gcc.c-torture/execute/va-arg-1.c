#include <stdarg.h>

void abort (void);
void exit (int);

typedef unsigned long L;
void
f (L p0, L p1, L p2, L p3, L p4, L p5, L p6, L p7, L p8, ...)
{
  va_list select;

  va_start (select, p8);

  if (va_arg (select, L) != 10)
    abort ();
  if (va_arg (select, L) != 11)
    abort ();
  if (va_arg (select, L) != 0)
    abort ();

  va_end (select);
}

int
main (void)
{
  f (1L, 2L, 3L, 4L, 5L, 6L, 7L, 8L, 9L, 10L, 11L, 0L);
  exit (0);
}
