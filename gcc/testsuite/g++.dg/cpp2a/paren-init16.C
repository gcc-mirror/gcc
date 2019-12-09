// PR c++/91363 - P0960R3: Parenthesized initialization of aggregates.
// { dg-do compile { target c++2a } }

struct S { int a[2]; };
struct A { S s[1]; };

template <typename, int N>
struct R { static constexpr auto h = A({S{N}}); };

template <typename, int N>
struct R2 { static constexpr auto h = A({S({N, N})}); };

A foo = R<int, 10>::h;
A foo2 = R2<int, 10>::h;
