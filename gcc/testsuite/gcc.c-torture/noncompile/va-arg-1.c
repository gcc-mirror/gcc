#include <stdarg.h>

f (int x, ...)
{
  va_list args;
  va_start (args, bogus_variable);
  va_end (args);
}
