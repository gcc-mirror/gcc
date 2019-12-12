// PR c++/80864
// { dg-do compile { target c++11 } }

struct S {int a[2]; };
struct A { S s[1]; };

template <typename, int N>
struct R { static constexpr auto h = A{S{N}}; };

template <typename, int N>
struct R2 { static constexpr auto h = A{S{{N, N}}}; };

A foo = R<int, 10>::h;
A foo2 = R2<int, 10>::h;
