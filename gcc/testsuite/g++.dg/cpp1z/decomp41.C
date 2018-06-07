// PR c++/85208
// { dg-do compile { target c++11 } }
// { dg-require-weak "" }
// { dg-options "" }

#pragma weak _ZDC1d1e1fE
struct A { int i, j, k; };
auto [a, b, c] = A ();	// { dg-warning "structured bindings only available with" "" { target c++14_down } }
auto [d, e, f] = A ();	// { dg-warning "structured bindings only available with" "" { target c++14_down } }
