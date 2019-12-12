// PR c++/57891
// { dg-do compile { target c++11 } }

struct X { constexpr operator int () { return 1000; } };
template<signed char> struct C {};
C<X{}> c; // { dg-error "narrowing conversion" }
