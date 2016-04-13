// { dg-options "-Wabi=9" }

#include <stdarg.h>

struct A { };

void f(int i, ...)
{
  va_list ap;
  va_start (ap, i);
  if (i >= 1)
    va_arg (ap, A);		// { dg-warning "ABI" }
  if (i >= 2)
    va_arg (ap, int);
}

int main()
{
  f(0);
  f(2, A(), 42);		// { dg-warning "ABI" }
}
