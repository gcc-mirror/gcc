// PR c++/58636
// { dg-do compile { target c++11 } }

#include <initializer_list>

// { dg-error "pointer to reference" "" { target *-*-* } 0 }
int foo(std::initializer_list<int&&>);

int i = foo({ 0 });		// { dg-error "std::initializer_list" }
