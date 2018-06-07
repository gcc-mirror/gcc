// PR c++/84540
// { dg-do compile { target c++11 } }

template<typename... T> struct A { enum alignas(alignof(T)...) E {}; };

A<> a;
