// { dg-do compile { target c++11 } }

using vfn_t = void();

template <vfn_t *T> struct A { };
template <vfn_t& T> struct B { };

[[deprecated("deprecated-global1")]] void fn1();
[[deprecated("deprecated-global2")]] void fn2();

A<fn1> a; // { dg-bogus "deprecated-global1.*deprecated-global1" }
// { dg-warning "deprecated-global1" "" { target *-*-* } .-1 }
B<fn2> b; // { dg-bogus "deprecated-global2.*deprecated-global2" }
// { dg-warning "deprecated-global2" "" { target *-*-* } .-1 }
