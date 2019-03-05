// PR c++/88373
// { dg-do compile }
// { dg-options "-std=c++2a" }

template <class T>
constexpr T value = T {};

template <class T, T t>
struct S {};

using U = S <int, ~value <int>>;
