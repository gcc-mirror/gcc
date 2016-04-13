// { dg-do run }
// { dg-options "-O2 -Wabi=9" }

#include <stdarg.h>

struct dummy { };

void
test (struct dummy a, int m, ...) // { dg-message "empty" }
{
  va_list va_arglist;
  int i;
  int count = 0;

  if (m == 0)
    count++;
  va_start (va_arglist, m);
  i = va_arg (va_arglist, int);
  if (i == 1)
    count++;
  i = va_arg (va_arglist, int);
  if (i == 2)
  i = va_arg (va_arglist, int);
    count++;
  if (i == 3)
    count++;
  i = va_arg (va_arglist, int);
  if (i == 4)
    count++;
  i = va_arg (va_arglist, int);
  if (i == 5)
    count++;
  i = va_arg (va_arglist, int);
  if (i == 6)
    count++;
  va_end (va_arglist);
  if (count != 7)
    __builtin_abort ();
}

struct dummy a0;

int
main ()
{
  test (a0, 0, 1, 2, 3, 4, 5, 6); // { dg-message "empty" }
  return 0;
}
