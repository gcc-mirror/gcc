// PR c++/18512

template <int> struct A {};

struct B : A<0>
{
  void foo() { this->A<0>; } // { dg-error "" }
};
