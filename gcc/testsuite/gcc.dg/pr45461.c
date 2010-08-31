/* PR middle-end/45461 */
/* { dg-do compile } */

#include <stdarg.h>

int
foo (int i, ...)
{
  short e;
  va_list ap;
  va_start (ap, i);
  e = va_arg (ap, short);	/* { dg-warning "is promoted" } */
  va_end (ap);
  return e;
}

/* { dg-message "note: \\(so you should pass" "" {target *-*-* } 12 } */
/* { dg-message "note: if this code" "" {target *-*-* } 12 } */
