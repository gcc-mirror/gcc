// PR c++/115207
// { dg-do compile { target c++17 } }

template <class T>
struct A {
  T t;
  constexpr A(T* p): t (p != &t) { }
};

const int i = 42;
constexpr A a = &i;
