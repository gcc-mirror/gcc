// PR c++/40619
// { dg-options "-std=c++11" }

template<typename U> struct X {};

template<typename T> auto f(T t) -> X<decltype(t+1)> {}
template<typename T> auto g(T t) -> X<decltype(t+1)> {}
