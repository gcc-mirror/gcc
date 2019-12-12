// PR c++/57891
// { dg-do compile { target c++11 } }
// { dg-options "-Wno-narrowing" }

template<unsigned int> struct A {};
A<-1> a;

template<signed char> struct B {};
B<1000> b; // { dg-warning "overflow" }
