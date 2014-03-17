// PR c++/60389
// { dg-do compile { target c++11 } }

struct A
{
  template<typename...T> A(T...) {}
};

struct B : A
{
  using A::A;   // { dg-error "inherited" }
};

constexpr B b;  // { dg-error "literal" }
