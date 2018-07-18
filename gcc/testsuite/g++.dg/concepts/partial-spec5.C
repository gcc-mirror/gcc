// PR c++/67138
// { dg-options "-std=c++17 -fconcepts" }

template <class T>
concept bool _Auto = true;

template <_Auto T>
struct test {};

template <_Auto T>
  requires requires (T t) { t + t; }
struct test<T> {};
