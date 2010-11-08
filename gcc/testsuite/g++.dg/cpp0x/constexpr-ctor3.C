// PR c++/46348
// { dg-options -std=c++0x }

struct A
{
  int arr[1];

  constexpr A()
  : arr() { }
};
