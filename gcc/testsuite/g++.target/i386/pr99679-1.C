// { dg-do compile }
// { dg-options "-Ofast -fipa-pta -mno-80387" }

#include <stdarg.h>

extern "C" void abort (void);

void
foo (int x, ...)
{
  long double ld;
  va_list ap;
  va_start (ap, x);
  ld = va_arg (ap, long double);
  if (ld)
    abort ();
} // { dg-error "x87 register return with x87 disabled" "" { target { ! ia32 } } }
