// PR c++/94508
// { dg-do compile { target c++20 } }

template <class T>
struct A {
  void f() requires (this, true) { }
};

template struct A<int>;
