// { dg-options "-Wno-varargs" }
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
    va_start (vp, a); // No warning because of -Wno-varargs in effect.
    va_end (vp);
}
