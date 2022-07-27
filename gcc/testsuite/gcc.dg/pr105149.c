/* PR c/105149 */
/* { dg-do compile } */
/* { dg-options "-O2" } */

#include <stdarg.h>

void
foo (int s, ...)
{
  int e;
  va_list ap;

  va_start (ap, s);
  e = va_arg (ap, int (void)) ();	/* { dg-error "second argument to 'va_arg' is a function type" } */
  va_end (ap);
}
