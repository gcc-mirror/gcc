/* PR target/87807 */
/* Reported by Rainer Orth <ro@gcc.gnu.org> */

/* { dg-do run } */
/* { dg-options "-std=c99" } */

#include <stdarg.h>

typedef double __attribute__ ((vector_size (16))) vector_double;

vector_double v2df = { 1.0, 2.0 };

void
f (vector_double a, ...)
{
  va_list argp;
  va_start (argp, a);
  vector_double x = va_arg (argp, vector_double);
  if (x[0] != a[0] || x[1] != a[1])
    __builtin_abort ();
  va_end (argp);
}

int
main (void)
{
  f (v2df, v2df);
  return 0;
} 
