// PR c++/67138
// { dg-do compile { target c++17_only } }
// { dg-options "-fconcepts-ts" }

template <class T>
concept bool _Auto = true;

template <_Auto T>
struct test {};

template <_Auto T>
  requires requires (T t) { t + t; }
struct test<T> {};
