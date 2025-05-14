// PR c++/119138
// { dg-do compile }

#include <stdarg.h>

template <typename>
void
foo (va_list)
{
}

void
bar (va_list ap)
{
  foo <char> (ap);
}
