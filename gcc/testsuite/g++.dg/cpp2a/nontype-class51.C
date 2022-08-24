// PR c++/103631
// { dg-do compile { target c++20 } }

template<class Target, template<auto> class T>
constexpr bool is_specialize_value_v = false;

template<template<auto> class T, auto Ts>
constexpr bool is_specialize_value_v<T<Ts>, T> = true;

template<class Target, template<auto> class T>
concept specialize_value = is_specialize_value_v<Target, T>;

template<int> struct Test { };

template<Test>
struct A {
  template<class T> void f(T) requires specialize_value<T, A>;
};

int main() {
  A<Test<0>{}> a0;
  A<Test<1>{}> a1;
  a0.f(a0);
  a0.f(a1);
  a0.f(0); // { dg-error "no match" }
}
