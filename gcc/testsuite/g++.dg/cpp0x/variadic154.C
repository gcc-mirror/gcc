// PR c++/60374
// { dg-do compile { target c++11 } }

template<typename> struct A {};

template<typename...T> struct A<T::T...> {}; // { dg-error "typename|partial|T" }

A<int> a;
