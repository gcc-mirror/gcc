// P3348R4 - C++26 should refer to C23 not C17
// { dg-do compile { target c++26 } }
// { dg-additional-options "-O2" }

#include <stdarg.h>

void
f (...)
{
  va_start ();	// { dg-error "expected primary-expression before '\\\)' token" }
}
