// PR c++/28058

template<int> struct A
{
  A() {}
};

A<0> a;

template<> A<0>::A() {} // { dg-error "specialization|invalid" } 
