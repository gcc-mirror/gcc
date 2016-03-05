// PR c++/70084
// { dg-do compile }

#include <stdarg.h>

struct A
{
  A (const char *f, ...);
};

A::A (const char *f, ...)
{
  va_list ap;
  va_start (ap, f);
  int i = va_arg (ap, int);	// { dg-bogus "first argument to 'va_arg' not of type 'va_list'" }
  int j = va_arg ((ap), int);	// { dg-bogus "first argument to 'va_arg' not of type 'va_list'" }
  va_end (ap);
}
