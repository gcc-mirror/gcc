// { dg-do compile { target c++11 } }
// { dg-options "-Wno-abi" { target arm_eabi } }
#include <stdarg.h>

struct S { };
void f(S const &);

void g(va_list args)
{
  f(va_arg(args, S));
}
