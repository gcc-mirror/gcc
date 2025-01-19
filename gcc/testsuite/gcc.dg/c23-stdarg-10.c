/* PR c/107980 */
/* { dg-do compile } */
/* { dg-options "-std=c23 -pedantic-errors" } */

#include <stdarg.h>

int i;

void
f0 (...)
{
  va_list ap;
  va_start (ap);
  va_end (ap);
}

void
f1 (...)
{
  va_list ap;
  va_start (ap, i);				/* { dg-warning "optional second parameter of 'va_start' not last named argument" } */
  va_end (ap);
}

void
f2 (...)
{
  int j = 0;
  va_list ap;
  va_start (ap, j);				/* { dg-warning "optional second parameter of 'va_start' not last named argument" } */
  va_end (ap);
}

void
f3 (int k, int l, ...)
{
  va_list ap;
  va_start (ap, k);				/* { dg-warning "optional second parameter of 'va_start' not last named argument" } */
  va_end (ap);
}

void
f4 (int k, int l, ...)
{
  va_list ap;
  va_start (ap, l);
  va_end (ap);
}

void
f5 (int k, int l, ...)
{
  va_list ap;
  va_start (ap, (int) l);			/* { dg-warning "'va_start' macro used with additional arguments other than identifier of the last named argument" } */
  va_end (ap);
}

void
f6 (int k, int l, ...)
{
  va_list ap;
  va_start (ap, l + 0);				/* { dg-warning "'va_start' macro used with additional arguments other than identifier of the last named argument" } */
  va_end (ap);
}

void
f7 (int k, int l, ...)
{
  va_list ap;
  va_start (ap, ()()(), [][][], {}{}{}, *+-/1({[_*_]})%&&!?!?);	/* { dg-warning "'va_start' macro used with additional arguments other than identifier of the last named argument" } */
  va_end (ap);
}

void
f8 (...)
{
  va_list ap;
  va_start (ap,);				/* { dg-warning "'va_start' macro used with additional arguments other than identifier of the last named argument" } */
  va_end (ap);
}

void
f9 (int k, int l, ...)
{
  va_list ap;
  va_start (ap, k+l+****2);			/* { dg-warning "'va_start' macro used with additional arguments other than identifier of the last named argument" } */
  va_end (ap);
}

void
f10 (register int m, ...)
{
  va_list ap;
  va_start (ap, m);				/* { dg-warning "undefined behavior when second parameter of 'va_start' is declared with 'register' storage" } */
  va_end (ap);
}

void
f11 (int k, int l, ...)
{
  va_list ap;
  va_start (ap, ()()()[[[}}});			/* { dg-warning "'va_start' macro used with additional arguments other than identifier of the last named argument" } */
  va_end (ap);
}

void
f12 (int k, int l, ...)
{
  va_list ap;
  va_start (ap, ]]]]]]{{{{{{);			/* { dg-warning "'va_start' macro used with additional arguments other than identifier of the last named argument" } */
  va_end (ap);
}
