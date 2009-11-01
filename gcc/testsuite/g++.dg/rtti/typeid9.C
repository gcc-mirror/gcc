// Test that the typeid name for a local class is properly null-terminated.
// { dg-do run }

#include <string.h>
#include <typeinfo>
#include <stdio.h>

int f()
{
  struct A {}; struct B {};
  const std::type_info &ti = typeid(A);
  const std::type_info &ti2 = typeid(B);
  puts (ti.name());
  puts (ti2.name());
  return strcmp (ti.name(), "Z1fvE1A") || strcmp (ti2.name(), "Z1fvE1B");
}

int main()
{
  return f();
}
