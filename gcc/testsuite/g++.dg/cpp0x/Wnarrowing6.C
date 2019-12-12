// PR c++/57891
// { dg-do compile { target c++11 } }

template<unsigned int> struct A {};
A<-1> a; // { dg-error "narrowing conversion" }

template<signed char> struct B {};
B<1000> b; // { dg-error "narrowing conversion" }
