/* This used to segfault on SPARC 64-bit at runtime because
   the stack pointer was clobbered by the function call.   */

/* { dg-do run } */

#include <stdarg.h>

union U
{
  long l1[2];
};

union U u;

void foo (int z, ...)
{
  int i;
  va_list ap;
  va_start(ap,z);
  i = va_arg(ap, int);
  va_end(ap);
}

int main(void)
{
  foo (1, 1, 1, 1, 1, u);
  return 0;
}
