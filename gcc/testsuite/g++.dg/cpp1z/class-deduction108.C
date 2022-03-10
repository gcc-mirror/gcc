// PR c++/102137
// { dg-do compile { target c++17 } }

template<class T>
struct A {
  constexpr A() { }
  constexpr A(int) { }
};

explicit A(...) -> A<int>;

template<template<class> class TT>
void f() {
  TT x1 = 0; // { dg-error "deduction|no match" }
  TT x2 = {0}; // { dg-error "explicit deduction guide" }
  TT x3(0);
  TT x4{0};
  TT x5;
  new TT(0);
  new TT{0};
  new TT();
  new TT{};
  new TT;
}

template<class T>
void g(T t) {
  A a1 = t; // { dg-error "deduction|no match" }
  A a2 = {t}; // { dg-error "explicit deduction guide" }
  A a3(t);
  A a4{t};
  A a5;
  new A(t);
  new A{t};
}

template void f<A>();
template void g(int);

template<template<class> class TT>
struct B {
  static inline TT x1 = 0; // { dg-error "deduction|no match" }
  static inline TT x2 = {0}; // { dg-error "explicit deduction guide" }
  static inline TT x4{0};
  static inline TT x5;
};

template<class T>
struct C {
  static inline T t;
  static inline A a1 = t; // { dg-error "deduction|no match" }
  static inline A a2 = {t}; // { dg-error "explicit deduction guide" }
  static inline A a4{t};
  static inline A a5{};
};

template struct B<A>;
template struct C<int>;

template<template<class> class TT>
struct E {
  static constexpr TT x1 = 0; // { dg-error "deduction|no match" }
  static constexpr TT x2 = {0}; // { dg-error "explicit deduction guide" }
  static constexpr TT x4{0};
  static constexpr TT x5{};
};

template<class T>
struct F {
  static constexpr T t{};
  static constexpr A a1 = t; // { dg-error "deduction|no match" }
  static constexpr A a2 = {t}; // { dg-error "explicit deduction guide" }
  static constexpr A a4{t};
  static constexpr A a5{};
};

template struct E<A>;
template struct F<int>;
