// { dg-options "--std=c++0x" }
#include <stdarg.h>

struct S { };
void f(S const &);

void g(va_list args)
{
  f(va_arg(args, S));
}
