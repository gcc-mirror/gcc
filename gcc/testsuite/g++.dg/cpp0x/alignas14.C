// PR c++/84540
// { dg-do compile { target c++11 } }

template<typename... T> struct alignas(alignof(T)...) A {};

A<> a;
