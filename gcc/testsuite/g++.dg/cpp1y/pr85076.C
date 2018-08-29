// PR c++/85076
// { dg-do compile { target c++14 } }

template<typename> struct A*;	// { dg-error "expected unqualified-id before" }

auto a = [](A<auto>) {};	// { dg-error "is not a template|has incomplete type" }
