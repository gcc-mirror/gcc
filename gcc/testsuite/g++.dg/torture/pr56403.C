// PR c++/56403
// { dg-do compile }

#include <stdarg.h>

struct S { va_list err_args; };

void *
foo ()
{
  return new S ();
}
