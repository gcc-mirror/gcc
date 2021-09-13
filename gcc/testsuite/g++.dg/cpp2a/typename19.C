// PR c++/99803
// { dg-do compile { target c++20 } }

struct A { template<typename T> A(T); };
auto A(unsigned) -> A::template A<int>; // { dg-error "not a type" }
