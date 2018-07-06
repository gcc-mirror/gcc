// PR c++/85133
// { dg-additional-options "-std=c++17 -fconcepts" }

template<typename> concept bool C; // { dg-error "no initializer" }

template<C...> struct A {};

A<int> a;
