// PR 98297, ICE
// { dg-do compile { target c++11 } }
template <template <class> class a>
struct [[b]]
a <int>; // { dg-error "name of class shadows template template parameter" }
// { dg-warning "ignored" "" { target *-*-* } .-1 }
