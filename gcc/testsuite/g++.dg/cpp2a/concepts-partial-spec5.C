// PR c++/67138
// { dg-do compile { target c++20 } }

template <class T>
concept Auto = true;

template <Auto T>
struct test {};

template <Auto T>
  requires requires (T t) { t + t; }
struct test<T> {};
