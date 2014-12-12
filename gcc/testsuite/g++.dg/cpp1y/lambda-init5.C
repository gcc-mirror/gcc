// Test for paren and brace initializers
// { dg-do run { target c++14 } }

#include <initializer_list>

int main()
{
  if ([x(42)]{ return x; }() != 42) __builtin_abort();
  if ([x{24}]{ return x; }() != 24) __builtin_abort();
}
