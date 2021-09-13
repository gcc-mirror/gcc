// { dg-additional-options -fmodules-ts }
module;
#include <stdarg.h>
export module builtins;
// { dg-module-cmi builtins }

export inline unsigned length (char const *ptr)
{
  return __builtin_strlen (ptr);
}

export inline int count (int a, ...)
{
  int c = 0;

  va_list args;
  va_start (args, a);
  while (va_arg (args, char *))
    c++;
  va_end (args);

  return c;
}
