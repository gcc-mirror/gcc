// PR c++/60336
// { dg-options "-Wabi=11" }

#include <stdarg.h>

struct A { };

void f(int i, ...)
{
  va_list ap;
  va_start (ap, i);
  if (i >= 1)
    va_arg (ap, A);
  if (i >= 2)
    va_arg (ap, int);
}

int main()
{
  f(0);
  f(1, A()); // { dg-warning "ABI" "" { target { { i?86-*-* x86_64-*-* } && { ! { ia32 } } } } }
  f(2, A(), 42); // { dg-warning "ABI" "" { target { { i?86-*-* x86_64-*-* } && { ! { ia32 } } } } }
}
