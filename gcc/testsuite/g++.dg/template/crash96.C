// PR c++/40406

template<int> struct A
{
  template<int> template<int> void A::foo() {} // { dg-error "extra qualification" }
};
