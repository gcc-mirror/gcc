// Test for paren and brace initializers
// { dg-do run { target c++1y } }

#include <initializer_list>

int main()
{
  if ([x(42)]{ return x; }() != 42) __builtin_abort();
  if ([x{1,2}]{ return x.begin()[0]; }() != 1) __builtin_abort();
}
