// PR c++/118060
// { dg-do compile { target c++20 } }

int* f(int);

template<class T>
struct A {
  template<class U> requires requires (U u) { *f(u); }
  A(T, U);
};

A a{0, 0};
