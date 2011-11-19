// PR c++/32056

template <auto int T> struct A {}; // { dg-error "storage class specified|two or more" }
template <extern int T> struct B {}; // { dg-error "storage class specified" }
template <static int T> struct C {}; // { dg-error "storage class specified" }
template <register int T> struct D {}; // { dg-error "storage class specified" }
template <mutable int T> struct E {}; // { dg-error "storage class specified" }
