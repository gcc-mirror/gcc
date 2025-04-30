// PR c++/119981
// { dg-do compile { target c++20 } }

template<template<class> class P>
struct mp_copy_if{};

template<auto Fn>
struct g {
  template<class> struct fn{};
};

template<typename>
void test3() {
  mp_copy_if<g<[]{}>::template fn> b;
}

template void test3<int>();
