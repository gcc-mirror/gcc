// { dg-do compile }
// PR c++/17221

#include <cstddef>

template <int N> struct Bar;
template <> struct Bar<3> {};

template <class T>
struct Foo {
   Bar<offsetof(T, a) + 3> k;
};

struct A { int a; };

template struct Foo<A>;
