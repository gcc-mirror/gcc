// { dg-options "--std=c++0x" }
// { dg-options "-Wno-abi --std=c++0x" { target arm_eabi } }
#include <stdarg.h>

struct S { };
void f(S const &);

void g(va_list args)
{
  f(va_arg(args, S));
}
