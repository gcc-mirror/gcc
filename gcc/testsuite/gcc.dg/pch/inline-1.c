#include "inline-1.hp"
int bar(int a, int b)
{
  return foo(a) + b;
}

int baz(void)
{
  return foo(3);
}
