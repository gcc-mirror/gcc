// PR c++/46348
// { dg-options -std=c++11 }

struct A
{
  int arr[1];

  constexpr A()
  : arr() { }
};
