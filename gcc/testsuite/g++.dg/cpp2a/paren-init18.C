// PR c++/91363 - P0960R3: Parenthesized initialization of aggregates.
// { dg-do compile { target c++20 } }

struct A { int a, b, c; };
struct S { A a; };
constexpr S s{ A(1, 2, 3) };
static_assert (s.a.a == 1 && s.a.b == 2 && s.a.c == 3);
constexpr S s2 = { A(1, 2, 3) };
static_assert (s2.a.a == 1 && s2.a.b == 2 && s2.a.c == 3);
