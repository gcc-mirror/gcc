// PR c++/84770
// { dg-do compile { target c++11 } }

typedef int T;

template<T&...> struct A {};

int i;

A<i> a;
