#include "inline-2.h"
extern inline char
bar(int a)
{
  return foo(a)[0];
}

extern inline char
baz(void)
{
  return foo(0)[0];
}
