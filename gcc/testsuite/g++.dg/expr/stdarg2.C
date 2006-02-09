// PR target/26141

#include <stdarg.h>

struct S
{
  double a;
};

void
foo (int z, ...)
{
  struct S arg;
  va_list ap;
  arg = va_arg (ap, struct S);
}


struct T
{
  __complex__ float a;
};

void
bar (int z, ...)
{
  struct T arg;
  va_list ap;
  arg = va_arg (ap, struct T);
}
