// PR c++/46348
// { dg-do compile { target c++11 } }

struct A
{
  int arr[1];

  constexpr A()
  : arr() { }
};
