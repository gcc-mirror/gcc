// PR c++/34892

template<int=..., int=0> struct A {};  // { dg-error "expected primary" }

A<0> a;
