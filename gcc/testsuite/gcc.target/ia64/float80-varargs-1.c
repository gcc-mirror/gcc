/* Test for a bug with passing __float80 in varargs.  The __float80
   value was wrongly passed, leading to an abort.  */
/* Origin: Joseph Myers <joseph@codesourcery.com> */
/* { dg-do run } */
/* { dg-options "" } */

#include <stdarg.h>

extern void abort (void);
extern void exit (int);

__float80 s = 1.234L;
__float80 d;

void vf (int a0, ...);

int
main (void)
{
  vf (0, s);
  if (d != s)
    abort ();
  exit (0);
}

void
vf (int a0, ...)
{
  va_list ap;
  va_start (ap, a0);
  d = va_arg (ap, __float80);
  va_end (ap);
}
