// PR c++/60389
// { dg-do compile { target c++11 } }

struct A
{
  template<typename...T> A(T...) {}
};

struct B : A
{
  using A::A;
};

constexpr B b;  // { dg-error "literal" "" { target { ! implicit_constexpr } } }
