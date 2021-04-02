// { dg-do compile }
// { dg-options "-Ofast -fipa-pta -mgeneral-regs-only" }

#include <stdarg.h>

extern "C" void abort (void);

void
foo (int x, ...)
{
  double ld;
  va_list ap;
  va_start (ap, x);
  ld = va_arg (ap, double); // { dg-error "SSE register argument with SSE disabled" "" { target { ! ia32 } } }
  if (ld)
    abort ();
} // { dg-error "SSE register return with SSE disabled" "" { target { ! ia32 } } }
