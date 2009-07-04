// PR c++/40619
// { dg-options "-std=c++0x" }

template<typename U> struct X {};

template<typename T> auto f(T t) -> X<decltype(t+1)> {}
template<typename T> auto g(T t) -> X<decltype(t+1)> {}
