/* PR target/64979 */
/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-stdarg" } */

#include <stdarg.h>

void bar (int x, va_list *ap);

void
foo (int x, ...)
{
  va_list ap;
  int n;

  va_start (ap, x);
  n = va_arg (ap, int);
  bar (x, (va_list *) ((n == 0) ? ((void *) 0) : &ap));
  va_end (ap);
}

/* { dg-final { scan-tree-dump "foo: va_list escapes 1, needs to save all GPR units and all FPR units" "stdarg" } } */
