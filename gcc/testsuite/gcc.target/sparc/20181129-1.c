/* PR target/87807 */
/* Reported by Rainer Orth <ro@gcc.gnu.org> */

/* { dg-do run } */
/* { dg-options "-std=c99" } */

#include <stdarg.h>

typedef float __attribute__ ((vector_size (8))) vector_float;

vector_float v2sf = { 1.0f, 2.0f };

void
f (vector_float a, ...)
{
  va_list argp;
  va_start (argp, a);
  vector_float x = va_arg (argp, vector_float);
  if (x[0] != a[0] || x[1] != a[1])
    __builtin_abort ();
  va_end (argp);
}

int
main (void)
{
  f (v2sf, v2sf);
  return 0;
} 
