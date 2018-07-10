// PR c++/32056

template <auto int T> struct A {}; // { dg-error "11:storage class specified" "" { target c++98_only } }
// { dg-error "two or more" "" { target c++11 } .-1 }
template <extern int T> struct B {}; // { dg-error "11:storage class specified" }
template <static int T> struct C {}; // { dg-error "11:storage class specified" }
template <register int T> struct D {}; // { dg-error "11:storage class specified" }
template <mutable int T> struct E {}; // { dg-error "11:storage class specified" }
