// { dg-do compile { target c++14 } }
// { dg-additional-options "-fconcepts" }

template<T> concept bool C = true;  // { dg-error "has not been declared" }

template<C...> struct A;
