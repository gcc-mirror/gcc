/* { dg-do compile } */
/* { dg-options "-Wc++-compat" } */

#include <stdarg.h>

enum E { A };

extern void f2 (int);
void
f1 (int n, ...)
{
  va_list ap;

  va_start (ap, n);
  f2 (va_arg (ap, int));
  f2 (va_arg (ap, _Bool));	/* { dg-warning "promoted" } */
  f2 (va_arg (ap, enum E));	/* { dg-warning "promoted" } */
}

/* Match extra informative notes.  */
/* { dg-message "note:" "expected" { target *-*-* } 0 } */
