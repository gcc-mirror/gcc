/* PR c/107980 */
/* { dg-do compile } */
/* { dg-options "-std=c23 -pedantic-errors" } */

#include <stdarg.h>

void
f (...)
{
  va_start ();	/* { dg-error "expected expression before '\\\)' token" } */
}
