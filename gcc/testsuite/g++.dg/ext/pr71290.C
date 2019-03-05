// PR c++/71290 - [6/7 Regression] Flexible array member is not diagnosed
// with -pedantic

// { dg-do compile }
// { dg-options "-Wall -Wpedantic" }

struct A
{
  int i;
  int arr[];   // { dg-warning "7:ISO C\\+\\+ forbids flexible array member .arr." }
};

template <class T>
struct B {
  T n;
  T a[];       // { dg-warning "5:ISO C\\+\\+ forbids flexible array member .a." }
};
