// PR c++/67138
// { dg-do compile { target c++17 } }
// { dg-options "-fconcepts" }

template <class T>
concept _Auto = true;

template <_Auto T>
struct test {};

template <_Auto T>
  requires requires (T t) { t + t; }
struct test<T> {};
