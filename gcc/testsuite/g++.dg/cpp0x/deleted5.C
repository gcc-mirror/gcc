// PR c++/58582
// { dg-do compile { target c++11 } }

struct A
{
  template<int> void foo() = delete;
};

template<int> void A::foo() {} // { dg-error "redefinition" }

template void A::foo<0>();
