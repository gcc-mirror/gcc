// PR c++/85133
// { dg-do compile { target c++17 } }
// { dg-additional-options "-fconcepts" }

template<typename> concept bool C; // { dg-error "no initializer" }

template<C...> struct A {};

A<int> a;
