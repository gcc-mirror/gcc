// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection" }

constexpr int x = 42;
struct S { static constexpr int x = 20; template <int N> static constexpr int a = N; };
static_assert (S {}.template [:^^S::a:]<x> == 42);
