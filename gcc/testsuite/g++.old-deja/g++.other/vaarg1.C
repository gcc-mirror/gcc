// Build don't link:
// Origin: Mark Mitchell <mark@codesourcery.com>

#include <stdarg.h>
#include <stdio.h>

void f (int i, ...)
{
  va_list ap;

  va_start (ap, i);
  vprintf ("test", ap);
  va_end (ap);
}
