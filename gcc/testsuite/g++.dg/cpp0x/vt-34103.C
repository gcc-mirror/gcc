// { dg-options "-std=c++0x" }
// PR c++/34103
template<typename> struct A {};

template<typename...T> void foo(A<T>, A<T>); // { dg-error "parameter packs|T" }

template<typename...T> void foo(A<T>, A<T>) {} // { dg-error "parameter packs|T" }
