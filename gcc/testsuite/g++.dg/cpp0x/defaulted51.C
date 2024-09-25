// PR c++/85067
// { dg-do compile { target c++11 } }

template<int> struct A
{
  A();
  A(volatile A&) = default;  // { dg-error "defaulted" "" { target c++17_down } }
};

struct B
{
  A<0> a;
};

B b;
