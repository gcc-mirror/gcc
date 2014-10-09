#include <stdarg.h>

void
f (int x, ...)
{
  va_list args;
  va_start (args, bogus_variable);  /* { dg-error "undeclared|for each function|not last named" } */
  va_end (args);
}
