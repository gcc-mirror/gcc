#include "inline-1.h"
int bar(int a, int b)
{
  return foo(a) + b;
}

int baz(void)
{
  return foo(3);
}
