/* PR middle-end/45461 */
/* { dg-do compile } */

#include <stdarg.h>

int
foo (int i, ...)
{
  short e;
  va_list ap;
  va_start (ap, i);

  e = va_arg (ap, short);	/* { dg-warning "is promoted" "promoted" } */
  /* { dg-message "note: \\(so you should pass" "should pass" {target *-*-* } .-1 } */
  /* { dg-message "note: if this code" "if this code" {target *-*-* } .-2 } */
  
  va_end (ap);
  return e;
}

