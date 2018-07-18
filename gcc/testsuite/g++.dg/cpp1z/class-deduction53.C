// PR c++/85135
// { dg-do compile { target c++11 } }

template<int> struct A {};

auto foo() -> A;		// { dg-error "A" }

template<typename> struct B {};

auto foo() -> A;		// { dg-error "A" }
