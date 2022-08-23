// PR c++/104291
// { dg-do compile { target c++20 } }

struct A { int x; };

template<auto> struct B;
template<int N> struct B<A{N}> { }; // { dg-error "not deducible" }
