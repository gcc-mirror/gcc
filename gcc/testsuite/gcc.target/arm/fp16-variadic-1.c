/* Test promotion of __fp16 to double as arguments to variadic function.  */

/* { dg-do run } */
/* { dg-options "-mfp16-format=ieee" } */

#include <stdlib.h>
#include <stdarg.h>

extern int f (int n, ...);

int 
f (int n, ...)
{
  if (n == 2)
    {
      double xx, yy;
      va_list ap;
      va_start (ap, n);
      xx = va_arg (ap, double);
      yy = va_arg (ap, double);
      va_end (ap);
      if (xx == 42.0 && yy == -42.0)
	return 1;
    }
  return 0;
}

static __fp16 x = 42.0;
static __fp16 y = -42.0;

int
main (void)
{
  if (!f (2, x, y))
    abort ();
  return 0;
}
