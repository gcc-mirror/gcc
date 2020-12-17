// PR 98297, ICE
// { dg-do compile { target c++11 } }
template <template <class> class a>
struct [[b]]
a <int>; // { dg-error "does not declare anything" }
// { dg-warning "ignored" "" { target *-*-* } .-1 }
