/* { dg-do compile } */

#include <stdarg.h>

void
err (int a)
{
  va_list vp;
  va_start (vp, a); // { dg-error "used in function with fixed args" }
}

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wvarargs"

void
foo0 (int a, int b, ...)
{
    va_list vp;
    /* 'a' is not the last argument of the enclosing function, but
       don't warn because we are ignoring -Wvarargs.  */
    va_start (vp, a);
    va_end (vp);
}

void
foo1 (int a, register int b, ...)
{
    va_list vp;
    /* 'b' is declared with register storage, but don't warn
       because we are ignoring -Wvarargs.  */
    va_start (vp, b);
    va_end (vp);
}

#pragma GCC diagnostic pop

void
foo2 (int a, int b, ...)
{
    va_list vp;
    /* 'a' is not the last argument of the enclosing function, so
       warn.  */
    va_start (vp, a); /* { dg-warning "second parameter" } */
    va_end (vp);
}

void
foo3 (int a, register int b, ...)
{
    va_list vp;
    /* 'b' is declared with register storage, so warn.  */
    va_start (vp, b); /* { dg-warning "undefined behaviour" } */
    va_end (vp);
}
