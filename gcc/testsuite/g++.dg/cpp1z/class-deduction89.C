// PR c++/86439
// { dg-do compile { target c++17 } }

struct B { };
struct C { };

template<class T>
struct A {
  A(T, B);
};

template<class T>
A(T, C) -> A<T>;

A a(0, {});
