// ensure a default level assert with a failing predicate does not generate an
// error during runtime when the contracts mode is off
// { dg-do run }
// { dg-options "-std=c++2a -fcontracts -fcontract-mode=off" }
// { dg-output "returning from main" }
#include <cstdio>

int constexpr f()
{
  constexpr int x = 1;
  [[assert default: x < 0]];
  return x;
}

template<typename T> int k()
{
  int x = 1;
  [[assert default: x < 0]];
  return x;
}


int main()
{
  int x = 1;
  [[assert default: x < 0]];
  constexpr int x2 = f();
  int x3 = k<int>();

  printf ("returning from main\n");
  return 0;
}
