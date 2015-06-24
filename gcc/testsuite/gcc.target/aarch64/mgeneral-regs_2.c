/* { dg-options "-mgeneral-regs-only" } */

#include <stdarg.h>

typedef int int32x2_t __attribute__ ((__vector_size__ ((8))));

int
test (int i, ...)
{
  va_list argp;
  va_start (argp, i);
  int32x2_t x = (int32x2_t) {0, 1};
  x += va_arg (argp, int32x2_t); /* { dg-error "'-mgeneral-regs-only' is incompatible with vector varargs" } */
  return x[0] + x[1];
}
