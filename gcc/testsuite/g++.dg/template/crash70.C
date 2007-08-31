// PR c++/32113

template<int> struct A;

template<typename T> void foo (A<&T::template i>); // { dg-error "not a template" }

template void foo<A<0> > (A<0>); // { dg-error "does not match" }
