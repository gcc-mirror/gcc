// { dg-do compile }

import core.stdc.stdarg;

void
error (int a)
{
  va_list vp;
  va_start (vp, a); // { dg-error "used in function with fixed arguments" }
}

void
warn (int a, int b, ...)
{
    va_list vp;
    va_start (vp, a); // { dg-warning "second parameter" }
    va_end (vp);
}
