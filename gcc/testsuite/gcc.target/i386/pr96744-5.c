/* { dg-do compile { target { ! ia32 } } } */
/* { dg-options "-msse2" } */

#include <stdarg.h>

typedef int int32x2_t __attribute__ ((__vector_size__ ((8))));

__attribute__((__target__("general-regs-only")))
int
test (int i, ...)
{
  va_list argp;
  va_start (argp, i);
  int32x2_t x = (int32x2_t) {0, 1};
  x += va_arg (argp, int32x2_t); /* { dg-error "SSE register argument with SSE disabled" } */
  return x[0] + x[1];
}
